;;; agent-shell-manager.el --- Buffer manager for agent-shell -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Jethro Kuan

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:
;;
;; Provides a buffer manager with tabulated list view of all open agent-shell buffers,
;; showing folder name, session status, and other details.
;;
;; Features:
;; - View all agent-shell buffers in a tabulated list
;; - See real-time status (ready, working, waiting, initializing, killed)
;; - Kill, restart, or create new agent-shells
;; - Manage session modes
;; - View traffic logs for debugging
;; - Auto-refresh every 2 seconds
;; - Killed processes are displayed at the bottom in red
;;
;; Usage:
;;   M-x agent-shell-manager-toggle
;;
;; Key bindings in the manager buffer:
;;   RET   - Switch to agent-shell buffer
;;   g     - Refresh buffer list
;;   k     - Kill agent-shell process
;;   c     - Create new agent-shell
;;   r     - Restart agent-shell
;;   d     - Delete all killed buffers
;;   m     - Set session mode
;;   M     - Cycle session mode
;;   C-c C-c - Interrupt agent
;;   t     - View traffic logs
;;   l     - Toggle logging
;;   q     - Quit manager window

;;; Code:

(require 'agent-shell)
(require 'tabulated-list)

(defgroup agent-shell-manager nil
  "Buffer manager for agent-shell."
  :group 'agent-shell)

(defcustom agent-shell-manager-side 'bottom
  "Side of the frame to display the agent-shell manager.
Can be 'left, 'right, 'top, or 'bottom."
  :type '(choice (const :tag "Left" left)
          (const :tag "Right" right)
          (const :tag "Top" top)
          (const :tag "Bottom" bottom))
  :group 'agent-shell-manager)

(defvar agent-shell-manager-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'agent-shell-manager-goto)
    (define-key map (kbd "g") #'agent-shell-manager-refresh)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "k") #'agent-shell-manager-kill)
    (define-key map (kbd "c") #'agent-shell-manager-new)
    (define-key map (kbd "r") #'agent-shell-manager-restart)
    (define-key map (kbd "d") #'agent-shell-manager-delete-killed)
    (define-key map (kbd "m") #'agent-shell-manager-set-mode)
    (define-key map (kbd "M") #'agent-shell-manager-cycle-mode)
    (define-key map (kbd "C-c C-c") #'agent-shell-manager-interrupt)
    (define-key map (kbd "t") #'agent-shell-manager-view-traffic)
    (define-key map (kbd "l") #'agent-shell-manager-toggle-logging)
    map)
  "Keymap for `agent-shell-manager-mode'.")

(defvar-local agent-shell-manager--refresh-timer nil
  "Timer for auto-refreshing the buffer list.")

(defvar agent-shell-manager--global-buffer nil
  "The global manager buffer for agent-shell buffer list.")

(define-derived-mode agent-shell-manager-mode tabulated-list-mode "Agent-Shell-Buffers"
  "Major mode for listing agent-shell buffers.

Key bindings:
\\[agent-shell-manager-goto] - Switch to agent-shell buffer at point
\\[agent-shell-manager-refresh] - Refresh the buffer list
\\[agent-shell-manager-kill] - Kill the agent-shell process at point
\\[agent-shell-manager-new] - Create a new agent-shell
\\[agent-shell-manager-restart] - Restart the agent-shell at point
\\[agent-shell-manager-delete-killed] - Delete all killed agent-shell buffers
\\[agent-shell-manager-set-mode] - Set session mode for agent at point
\\[agent-shell-manager-cycle-mode] - Cycle session mode for agent at point
\\[agent-shell-manager-interrupt] - Interrupt the agent at point
\\[agent-shell-manager-view-traffic] - View traffic logs for agent at point
\\[agent-shell-manager-toggle-logging] - Toggle ACP logging
\\[quit-window] - Quit the manager window

\\{agent-shell-manager-mode-map}"
  (setq tabulated-list-format
        [("Folder" 60 t)
         ("Status" 15 t)
         ("Session" 10 t)
         ("Mode" 15 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Folder" nil))
  (tabulated-list-init-header)
  
  ;; Set up auto-refresh timer (refresh every 2 seconds)
  (setq agent-shell-manager--refresh-timer
        (run-with-timer 2 2 #'agent-shell-manager-refresh))
  
  ;; Cancel timer when buffer is killed
  (add-hook 'kill-buffer-hook
            (lambda ()
              (when agent-shell-manager--refresh-timer
                (cancel-timer agent-shell-manager--refresh-timer)
                (setq agent-shell-manager--refresh-timer nil)))
            nil t))

(defun agent-shell-manager--get-status (buffer)
  "Get the current status of agent-shell BUFFER.
Returns one of: waiting, ready, working, killed, or unknown."
  (with-current-buffer buffer
    (if (not (boundp 'agent-shell--state))
        "unknown"
      (let* ((state agent-shell--state)
             (acp-proc (map-nested-elt state '(:client :process)))
             (acp-process-alive (and acp-proc
                                     (processp acp-proc)
                                     (process-live-p acp-proc)
                                     ;; Additional check: process status should not be 'exit or 'signal
                                     (memq (process-status acp-proc) '(run open listen connect stop))))
             ;; Check the comint process (the actual shell process)
             (comint-proc (get-buffer-process (current-buffer)))
             (comint-process-alive (and comint-proc
                                        (processp comint-proc)
                                        (process-live-p comint-proc)
                                        (memq (process-status comint-proc) '(run open listen connect stop))))
             ;; Both processes must be alive for the shell to be truly alive
             (process-alive (and acp-process-alive comint-process-alive)))
        (cond
         ;; Check if either the ACP client or comint process is dead or missing
         ((and (map-elt state :client)
               (or (not acp-proc)
                   (not comint-proc)
                   (and (processp acp-proc)
                        (not acp-process-alive))
                   (and (processp comint-proc)
                        (not comint-process-alive))))
          "killed")
         ;; Check if buffer is busy (shell-maker function)
         ((and process-alive
               (fboundp 'shell-maker-busy)
               (shell-maker-busy))
          "working")
         ;; Check if there are pending tool calls
         ((and process-alive
               (map-elt state :tool-calls)
               (> (length (map-elt state :tool-calls)) 0))
          ;; Check if any tool call is pending permission
          (let ((has-pending-permission
                 (seq-find (lambda (tool-call)
                             (map-elt (cdr tool-call) :permission-request-id))
                           (map-elt state :tool-calls))))
            (if has-pending-permission
                "waiting"
              "working")))
         ;; Check if session is active (only if process is alive)
         ((and process-alive
               (map-nested-elt state '(:session :id)))
          "ready")
         ;; Still initializing
         ((not (map-elt state :initialized))
          "initializing")
         (t "unknown"))))))

(defun agent-shell-manager--get-folder (buffer)
  "Get the full folder path for BUFFER."
  (with-current-buffer buffer
    (abbreviate-file-name
     (string-remove-suffix "/" default-directory))))

(defun agent-shell-manager--get-session-status (buffer)
  "Get session status for BUFFER."
  (with-current-buffer buffer
    (let ((status (agent-shell-manager--get-status buffer)))
      (if (string= status "killed")
          "none"
        (if (and (boundp 'agent-shell--state)
                 (map-nested-elt agent-shell--state '(:session :id)))
            "active"
          "none")))))

(defun agent-shell-manager--get-session-mode (buffer)
  "Get the current session mode for BUFFER."
  (with-current-buffer buffer
    (if (and (boundp 'agent-shell--state)
             (map-nested-elt agent-shell--state '(:session :mode-id)))
        (or (agent-shell--resolve-session-mode-name
             (map-nested-elt agent-shell--state '(:session :mode-id))
             (map-nested-elt agent-shell--state '(:session :modes)))
            "-")
      "-")))

(defun agent-shell-manager--status-face (status)
  "Return face for STATUS string."
  (cond
   ((string= status "ready") 'success)
   ((string= status "working") 'warning)
   ((string= status "waiting") 'font-lock-keyword-face)
   ((string= status "initializing") 'font-lock-comment-face)
   ((string= status "killed") 'error)
   (t 'default)))

(defun agent-shell-manager--entries ()
  "Return list of entries for tabulated-list."
  (let* ((buffers (seq-filter
                   (lambda (buffer-name)
                     (buffer-live-p (get-buffer buffer-name)))
                   (agent-shell-buffers)))
         (entries (mapcar
                   (lambda (buffer-name)
                     (let* ((buffer (get-buffer buffer-name))
                            (folder (agent-shell-manager--get-folder buffer))
                            (status (agent-shell-manager--get-status buffer))
                            (session (agent-shell-manager--get-session-status buffer))
                            (mode (agent-shell-manager--get-session-mode buffer)))
                       (list buffer
                             (vector
                              folder
                              (propertize status 'face (agent-shell-manager--status-face status))
                              session
                              mode))))
                   buffers)))
    ;; Sort entries: killed processes go to the bottom
    (sort entries
          (lambda (a b)
            (let ((status-a (aref (cadr a) 1))
                  (status-b (aref (cadr b) 1)))
              ;; Remove text properties to get plain status string
              (setq status-a (substring-no-properties status-a))
              (setq status-b (substring-no-properties status-b))
              (cond
               ;; Both killed or both not killed - maintain original order (stable)
               ((and (string= status-a "killed") (string= status-b "killed")) nil)
               ((and (not (string= status-a "killed")) (not (string= status-b "killed"))) nil)
               ;; a is killed, b is not - a goes after b
               ((string= status-a "killed") nil)
               ;; b is killed, a is not - a goes before b
               (t t)))))))

(defun agent-shell-manager-refresh ()
  "Refresh the buffer list."
  (interactive)
  (when (and agent-shell-manager--global-buffer
             (buffer-live-p agent-shell-manager--global-buffer))
    (with-current-buffer agent-shell-manager--global-buffer
      (setq tabulated-list-entries (agent-shell-manager--entries))
      (tabulated-list-print t))))

(defun agent-shell-manager-goto ()
  "Go to the agent-shell buffer at point without closing the manager."
  (interactive)
  (when-let ((buffer (tabulated-list-get-id)))
    (if (buffer-live-p buffer)
        (agent-shell--display-buffer buffer)
      (user-error "Buffer no longer exists"))))

(defun agent-shell-manager-kill ()
  "Kill the agent-shell process at point."
  (interactive)
  (when-let ((buffer (tabulated-list-get-id)))
    (unless (buffer-live-p buffer)
      (user-error "Buffer no longer exists"))
    (when (yes-or-no-p (format "Kill agent-shell process in %s? " (buffer-name buffer)))
      (with-current-buffer buffer
        (when (and (boundp 'agent-shell--state)
                   (map-elt agent-shell--state :client)
                   (map-nested-elt agent-shell--state '(:client :process)))
          (let ((proc (map-nested-elt agent-shell--state '(:client :process))))
            (when (process-live-p proc)
              (comint-send-eof)
              (message "Sent EOF to agent-shell process in %s" (buffer-name buffer))))))
      ;; Give the process a moment to update its status before refreshing
      (run-with-timer 0.1 nil #'agent-shell-manager-refresh))))

(defun agent-shell-manager-new ()
  "Create a new agent-shell."
  (interactive)
  (agent-shell t)
  (agent-shell-manager-refresh))

(defun agent-shell-manager--get-buffer-config (buffer)
  "Try to determine the config used for BUFFER.
Returns nil if config cannot be determined."
  (with-current-buffer buffer
    ;; Try to match buffer name against known configs
    (when (derived-mode-p 'agent-shell-mode)
      (let ((buffer-name-prefix (replace-regexp-in-string " Agent @ .*$" "" (buffer-name))))
        (seq-find (lambda (config)
                    (string= buffer-name-prefix (map-elt config :buffer-name)))
                  agent-shell-agent-configs)))))

(defun agent-shell-manager-restart ()
  "Restart the agent-shell at point.
Kills the current process and starts a new one with the same config if possible."
  (interactive)
  (when-let ((buffer (tabulated-list-get-id)))
    (unless (buffer-live-p buffer)
      (user-error "Buffer no longer exists"))
    (let ((config (agent-shell-manager--get-buffer-config buffer))
          (buffer-name (buffer-name buffer)))
      (when (yes-or-no-p (format "Restart agent-shell %s? " buffer-name))
        ;; Kill the current process
        (with-current-buffer buffer
          (when (and (boundp 'agent-shell--state)
                     (map-elt agent-shell--state :client)
                     (map-nested-elt agent-shell--state '(:client :process)))
            (let ((proc (map-nested-elt agent-shell--state '(:client :process))))
              (when (process-live-p proc)
                (kill-process proc)))))
        ;; Kill the buffer
        (kill-buffer buffer)
        ;; Start a new one
        (if config
            (agent-shell-start :config config)
          (agent-shell t))
        (agent-shell-manager-refresh)
        (message "Restarted %s" buffer-name)))))

(defun agent-shell-manager-delete-killed ()
  "Delete all killed agent-shell buffers from the list."
  (interactive)
  (let ((killed-buffers (seq-filter
                         (lambda (buffer)
                           (and (buffer-live-p buffer)
                                (string= (agent-shell-manager--get-status buffer) "killed")))
                         (mapcar #'get-buffer (agent-shell-buffers)))))
    (if (null killed-buffers)
        (message "No killed buffers to delete")
      (when (yes-or-no-p (format "Delete %d killed buffer%s? "
                                 (length killed-buffers)
                                 (if (= (length killed-buffers) 1) "" "s")))
        (dolist (buffer killed-buffers)
          (kill-buffer buffer))
        (agent-shell-manager-refresh)
        (message "Deleted %d killed buffer%s"
                 (length killed-buffers)
                 (if (= (length killed-buffers) 1) "" "s"))))))

(defun agent-shell-manager-set-mode ()
  "Set session mode for the agent-shell at point."
  (interactive)
  (when-let ((buffer (tabulated-list-get-id)))
    (unless (buffer-live-p buffer)
      (user-error "Buffer no longer exists"))
    (with-current-buffer buffer
      (unless (derived-mode-p 'agent-shell-mode)
        (user-error "Not an agent-shell buffer"))
      (agent-shell-set-session-mode))
    (agent-shell-manager-refresh)))

(defun agent-shell-manager-cycle-mode ()
  "Cycle session mode for the agent-shell at point."
  (interactive)
  (when-let ((buffer (tabulated-list-get-id)))
    (unless (buffer-live-p buffer)
      (user-error "Buffer no longer exists"))
    (with-current-buffer buffer
      (unless (derived-mode-p 'agent-shell-mode)
        (user-error "Not an agent-shell buffer"))
      (agent-shell-cycle-session-mode))
    (agent-shell-manager-refresh)))

(defun agent-shell-manager-interrupt ()
  "Interrupt the agent-shell at point."
  (interactive)
  (when-let ((buffer (tabulated-list-get-id)))
    (unless (buffer-live-p buffer)
      (user-error "Buffer no longer exists"))
    (with-current-buffer buffer
      (unless (derived-mode-p 'agent-shell-mode)
        (user-error "Not an agent-shell buffer"))
      (agent-shell-interrupt))
    (agent-shell-manager-refresh)))

(defun agent-shell-manager-view-traffic ()
  "View traffic logs for the agent-shell at point."
  (interactive)
  (when-let ((buffer (tabulated-list-get-id)))
    (unless (buffer-live-p buffer)
      (user-error "Buffer no longer exists"))
    (with-current-buffer buffer
      (unless (derived-mode-p 'agent-shell-mode)
        (user-error "Not an agent-shell buffer"))
      (agent-shell-view-traffic))))

(defun agent-shell-manager-toggle-logging ()
  "Toggle logging for agent-shell."
  (interactive)
  (agent-shell-toggle-logging)
  (agent-shell-manager-refresh))

;;;###autoload
(defun agent-shell-manager-toggle ()
  "Toggle the agent-shell buffer list window.
Shows folder name, agent type, status (ready/waiting/working), session info, and mode.
The position of the window is controlled by `agent-shell-manager-side'."
  (interactive)
  (let* ((buffer (get-buffer-create "*Agent-Shell Buffers*"))
         (window (get-buffer-window buffer)))
    (if (and window (window-live-p window))
        ;; Window is visible, hide it
        (delete-window window)
      ;; Window is not visible, show it
      (let* ((size-param (if (memq agent-shell-manager-side '(left right))
                             'window-width
                           'window-height))
             (window (display-buffer-in-side-window
                      buffer
                      `((side . ,agent-shell-manager-side)
                        (slot . 0)
                        (,size-param . 0.3)
                        (preserve-size . ,(if (memq agent-shell-manager-side '(left right))
                                              '(t . nil)
                                            '(nil . t)))
                        (window-parameters . ((no-delete-other-windows . t)))))))
        (setq agent-shell-manager--global-buffer buffer)
        (with-current-buffer buffer
          (agent-shell-manager-mode)
          (agent-shell-manager-refresh))
        ;; Make the window dedicated so it can't be used for other buffers
        (set-window-dedicated-p window t)
        (select-window window)))))

(provide 'agent-shell-manager-buffer)

;;; agent-shell-manager-buffer.el ends here
