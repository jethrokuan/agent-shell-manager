;;; agent-shell-manager.el --- Buffer manager for agent-shell -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Jethro Kuan

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:
;;
;; Provides a buffer manager with tabulated list view of all open agent-shell buffers,
;; showing buffer name, session status, and other details.
;;
;; Features:
;; - View all agent-shell buffers in a tabulated list
;; - See real-time status (ready, working, waiting, initializing, killed)
;; - Kill, restart, or create new agent-shells
;; - Manage session modes
;; - View traffic logs for debugging
;; - Auto-refresh every 2 seconds
;; - Optional macOS notifications when agents become ready
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
;;   M     - Set session model
;;   C-c C-c - Interrupt agent
;;   t     - View traffic logs
;;   l     - Toggle logging
;;   w     - Set annotation
;;   q     - Quit manager window

;;; Code:

(require 'agent-shell)
(require 'tabulated-list)
(require 'subr-x)

(defgroup agent-shell-manager nil
  "Buffer manager for `agent-shell'."
  :group 'agent-shell)

(defcustom agent-shell-manager-side 'bottom
  "Side of the frame to display the `agent-shell' manager.
Can be 'left, 'right, 'top, 'bottom, or nil.  When nil, buffer display
is controlled by the user's `display-buffer-alist'."
  :type '(choice (const :tag "Left" left)
          (const :tag "Right" right)
          (const :tag "Top" top)
          (const :tag "Bottom" bottom)
          (const :tag "User-controlled" nil))
  :group 'agent-shell-manager)

(defcustom agent-shell-manager-transient nil
  "When non-nil, automatically hide the manager window after actions.
This includes switching to a shell buffer with RET.  When enabled,
the manager window can also be closed by `delete-other-windows' (C-x 1)."
  :type 'boolean
  :group 'agent-shell-manager)

(defcustom agent-shell-manager-side-window-side 'right
  "Side used by `agent-shell-manager-switch-to-side-window'."
  :type '(choice (const :tag "Left" left)
          (const :tag "Right" right))
  :group 'agent-shell-manager)

(defcustom agent-shell-manager-side-window-width 0.22
  "Width used by `agent-shell-manager-switch-to-side-window'."
  :type 'number
  :group 'agent-shell-manager)

(defcustom agent-shell-manager-side-window-min-width 36
  "Minimum width in columns for the manager side window."
  :type 'integer
  :group 'agent-shell-manager)

(defcustom agent-shell-manager-side-window-max-width 44
  "Maximum width in columns for the manager side window."
  :type 'integer
  :group 'agent-shell-manager)

(defcustom agent-shell-manager-side-window-columns '(buffer annotation status)
  "Columns shown by `agent-shell-manager-switch-to-side-window'."
  :type '(repeat symbol)
  :group 'agent-shell-manager)

(defcustom agent-shell-manager-side-window-column-widths
  '((buffer . 15)
    (annotation . 13)
    (status . 7))
  "Column widths used by `agent-shell-manager-switch-to-side-window'."
  :type '(alist :key-type symbol :value-type integer)
  :group 'agent-shell-manager)

(defcustom agent-shell-manager-ready-status-notifications t
  "When non-nil, send a macOS notification when status changes from working to ready.

Notifications are shown only when the manager is not visible or Emacs
is not the active application."
  :type 'boolean
  :group 'agent-shell-manager)

(defcustom agent-shell-manager-ready-status-notification-sound nil
  "When non-nil, include the default sound in ready status notifications.

This uses macOS notification sound support via AppleScript."
  :type 'boolean
  :group 'agent-shell-manager)

(defface agent-shell-manager-done
  '((t (:foreground "#16524F" :weight bold)))
  "Face for agents that completed since last visit."
  :group 'agent-shell-manager)

(defconst agent-shell-manager--column-specs
  '((buffer "Buffer" 40 t)
    (provider "Provider" 12 t)
    (status "Status" 15 t)
    (mode "Mode" 15 t)
    (model "Model" 21 t)
    (pending-permissions "Pending Permissions" 20 t)
    (annotation "Annotation" 25 t)
    (path "Path" 20 t)
    (last-command "Last Command" 20 t))
  "Specs for columns available in `agent-shell-manager-visible-columns'.")

(defcustom agent-shell-manager-visible-columns
  '(buffer status mode model annotation last-command)
  "Columns shown in the manager table.

The order of this list controls display order."
  :type '(repeat
          (choice (const :tag "Buffer" buffer)
                  (const :tag "Provider" provider)
                  (const :tag "Status" status)
                  (const :tag "Mode" mode)
                  (const :tag "Model" model)
                  (const :tag "Pending Permissions" pending-permissions)
                  (const :tag "Annotation" annotation)
                  (const :tag "Path" path)
                  (const :tag "Last Command" last-command)))
  :group 'agent-shell-manager)

(defun agent-shell-manager--column-spec (column)
  "Return column spec for COLUMN, or nil if invalid."
  (assoc column agent-shell-manager--column-specs))

(defun agent-shell-manager--visible-columns ()
  "Return sanitized list of visible columns.

Invalid or duplicated entries are removed."
  (let (columns)
    (dolist (column agent-shell-manager-visible-columns)
      (when (and (agent-shell-manager--column-spec column)
                 (not (memq column columns)))
        (push column columns)))
    (or (nreverse columns) '(buffer))))

(defun agent-shell-manager--tabulated-list-format (columns &optional widths)
  "Build `tabulated-list-format' vector for COLUMNS."
  (vconcat
   (mapcar (lambda (column)
             (let ((spec (agent-shell-manager--column-spec column)))
               (list (nth 1 spec)
                     (or (alist-get column widths) (nth 2 spec))
                     (nth 3 spec))))
           columns)))

(defun agent-shell-manager--column-width (column &optional widths)
  "Return display width for COLUMN using WIDTHS override when present."
  (let ((spec (agent-shell-manager--column-spec column)))
    (or (alist-get column widths) (nth 2 spec))))

(defun agent-shell-manager--single-line-cell (value)
  "Return VALUE as a one-line string."
  (replace-regexp-in-string "[\n\r\t]+" " "
                            (if (stringp value) value (format "%s" value))))

(defun agent-shell-manager--truncate-cell (value width)
  "Truncate VALUE to WIDTH columns without shifting later columns."
  (let ((cell (agent-shell-manager--single-line-cell value)))
    (if (and width (> (string-width cell) width))
        (truncate-string-to-width cell width nil nil "...")
      cell)))

(defun agent-shell-manager--column-names (columns)
  "Return display names for COLUMNS."
  (mapcar (lambda (column)
            (nth 1 (agent-shell-manager--column-spec column)))
          columns))

(defun agent-shell-manager--apply-column-configuration (&optional widths)
  "Apply visible column configuration in the current manager buffer."
  (let* ((columns (agent-shell-manager--visible-columns))
         (column-names (agent-shell-manager--column-names columns))
         (new-format (agent-shell-manager--tabulated-list-format columns widths))
         (current-sort-column (car-safe tabulated-list-sort-key)))
    (unless (equal tabulated-list-format new-format)
      (setq tabulated-list-format new-format)
      (tabulated-list-init-header))
    (unless (member current-sort-column column-names)
      (setq tabulated-list-sort-key (cons (car column-names) nil)))
    columns))

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
    (define-key map (kbd "M") #'agent-shell-manager-set-model)
    (define-key map (kbd "C-c C-c") #'agent-shell-manager-interrupt)
    (define-key map (kbd "t") #'agent-shell-manager-view-traffic)
    (define-key map (kbd "l") #'agent-shell-manager-toggle-logging)
    (define-key map (kbd "w") #'agent-shell-manager-set-annotation)
    (define-key map (kbd "v") #'agent-shell-manager-switch-to-side-window)
    (define-key map (kbd "V") #'agent-shell-manager-switch-to-default-window)
    map)
  "Keymap for `agent-shell-manager-mode'.")

(defvar-local agent-shell-manager--refresh-timer nil
  "Timer for auto-refreshing the buffer list.")

(defvar agent-shell-manager--global-buffer nil
  "The global manager buffer for `agent-shell' buffer list.")

(defvar-local agent-shell-manager--annotation nil
  "User annotation for this `agent-shell' buffer.")

(defvar-local agent-shell-manager--last-command nil
  "Most recent slash command submitted in this `agent-shell' buffer.")

(defvar-local agent-shell-manager--model-id nil
  "Most recent session model ID known by the manager for this buffer.")

(defvar-local agent-shell-manager--mode-id nil
  "Most recent session mode ID known by the manager for this buffer.")

(defvar agent-shell-manager--status-history (make-hash-table :test #'eq)
  "Hash table tracking last known status per agent-shell buffer.")

(defvar agent-shell-manager--done-unseen (make-hash-table :test #'eq)
  "Hash table tracking agent buffers completed since last visit.")

(defvar agent-shell-manager--default-visible-columns nil
  "Visible columns to restore after using compact side-window layout.")

(defvar-local agent-shell-manager--column-widths nil
  "Buffer-local column widths overriding `agent-shell-manager--column-specs'.")

(defvar agent-shell-manager--notification-timer nil
  "Timer for polling shell statuses and emitting ready notifications.")

(define-derived-mode agent-shell-manager-mode tabulated-list-mode "Agent-Shell-Buffers"
  "Major mode for listing `agent-shell' buffers.

Key bindings:
\\[agent-shell-manager-goto] - Switch to `agent-shell' buffer at point
\\[agent-shell-manager-refresh] - Refresh the buffer list
\\[agent-shell-manager-kill] - Kill the `agent-shell' process at point
\\[agent-shell-manager-new] - Create a new `agent-shell'
\\[agent-shell-manager-restart] - Restart the `agent-shell' at point
\\[agent-shell-manager-delete-killed] - Delete all killed `agent-shell' buffers
\\[agent-shell-manager-set-mode] - Set session mode for agent at point
\\[agent-shell-manager-set-model] - Set session model for agent at point
\\[agent-shell-manager-interrupt] - Interrupt the agent at point
\\[agent-shell-manager-view-traffic] - View traffic logs for agent at point
\\[agent-shell-manager-toggle-logging] - Toggle ACP logging
\\[agent-shell-manager-set-annotation] - Set annotation for agent at point
\\[quit-window] - Quit the manager window

\\{agent-shell-manager-mode-map}"
  (setq tabulated-list-padding 2)
  (agent-shell-manager--apply-column-configuration
   agent-shell-manager--column-widths)

  (when agent-shell-manager--refresh-timer
    (cancel-timer agent-shell-manager--refresh-timer))

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
  "Get the current status of `agent-shell' BUFFER.
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
         ;; Check if comint process is dead or missing - if so, always report killed
         ((or (not comint-proc)
              (and (processp comint-proc)
                   (not comint-process-alive)))
          "killed")
         ;; Check if ACP client process is dead or missing (when client exists)
         ((and (map-elt state :client)
               (or (not acp-proc)
                   (and (processp acp-proc)
                        (not acp-process-alive))))
          "killed")
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
         ;; Check if buffer is busy (shell-maker function)
         ((and process-alive
               (fboundp 'shell-maker-busy)
               (shell-maker-busy))
          "working")
         ;; Check if session is active (only if process is alive)
         ((and process-alive
               (map-nested-elt state '(:session :id)))
          "ready")
         ;; Still initializing
         ((not (map-elt state :initialized))
          "initializing")
         (t "unknown"))))))

(defun agent-shell-manager--manager-visible-p ()
  "Return non-nil when manager buffer is visible in any window."
  (and agent-shell-manager--global-buffer
       (buffer-live-p agent-shell-manager--global-buffer)
       (get-buffer-window agent-shell-manager--global-buffer t)))

(defun agent-shell-manager--buffer-visible-p (buffer)
  "Return non-nil when BUFFER is visible in any window."
  (and (buffer-live-p buffer)
       (get-buffer-window buffer t)))

(defun agent-shell-manager--buffer-selected-p (buffer)
  "Return non-nil when BUFFER is selected in the current frame."
  (and (buffer-live-p buffer)
       (eq (window-buffer (selected-window)) buffer)))

(defun agent-shell-manager--done-unseen-p (buffer)
  "Return non-nil when BUFFER completed since last visit."
  (gethash buffer agent-shell-manager--done-unseen))

(defun agent-shell-manager--clear-done-unseen (buffer)
  "Clear unseen completion state for BUFFER."
  (when (gethash buffer agent-shell-manager--done-unseen)
    (remhash buffer agent-shell-manager--done-unseen)
    (agent-shell-manager-refresh)))

(defun agent-shell-manager--mark-current-agent-visited ()
  "Clear unseen completion state when visiting an `agent-shell' buffer."
  (when (derived-mode-p 'agent-shell-mode)
    (agent-shell-manager--clear-done-unseen (current-buffer))))

(defun agent-shell-manager--emacs-active-p ()
  "Return non-nil when Emacs appears to be the active app.

If focus state can't be determined, returns non-nil."
  (if (fboundp 'frame-focus-state)
      (let ((focus (frame-focus-state)))
        (not (null focus)))
    t))

(defun agent-shell-manager--send-macos-notification (title message)
  "Send macOS notification with TITLE and MESSAGE."
  (when (eq system-type 'darwin)
    (let* ((script (if agent-shell-manager-ready-status-notification-sound
                       (format "display notification %S with title %S sound name %S"
                               message title "default")
                     (format "display notification %S with title %S"
                             message title)))
           (osascript (executable-find "osascript")))
      (cond
       ((fboundp 'do-applescript)
        (condition-case err
            (do-applescript script)
          (error
           (message "agent-shell-manager notification failed: %s"
                    (error-message-string err)))))
       ((fboundp 'ns-do-applescript)
        (condition-case err
            (ns-do-applescript script)
          (error
           (message "agent-shell-manager notification failed: %s"
                    (error-message-string err)))))
       (osascript
        (with-temp-buffer
          (unless (zerop (call-process osascript nil t nil "-e" script))
            (message "agent-shell-manager notification failed: %s"
                     (string-trim (buffer-string))))))
       (t
        (message "agent-shell-manager notification failed: no AppleScript backend found"))))))

(defun agent-shell-manager--should-notify-ready-transition-p (buffer)
  "Return non-nil when BUFFER ready notifications should be emitted now."
  (and agent-shell-manager-ready-status-notifications
       (or (not (agent-shell-manager--emacs-active-p))
           (and (not (agent-shell-manager--manager-visible-p))
                (not (agent-shell-manager--buffer-visible-p buffer))))))

(defun agent-shell-manager--notification-label (buffer)
  "Return notification label for BUFFER.

Includes annotation when present to help identify shell scope."
  (with-current-buffer buffer
    (let ((annotation (and (stringp agent-shell-manager--annotation)
                           (string-trim agent-shell-manager--annotation))))
      (if (and annotation (not (string-empty-p annotation)))
          (format "%s (%s)" (buffer-name buffer) annotation)
        (buffer-name buffer)))))

(defun agent-shell-manager--maybe-notify-ready-transition (buffer current-status)
  "Notify if BUFFER transitioned from working to ready.
CURRENT-STATUS should be the raw status string."
  (let ((previous-status (gethash buffer agent-shell-manager--status-history)))
    (puthash buffer current-status agent-shell-manager--status-history)
    (when (agent-shell-manager--buffer-selected-p buffer)
      (remhash buffer agent-shell-manager--done-unseen))
    (when (and (equal previous-status "working")
               (equal current-status "ready"))
      (if (agent-shell-manager--buffer-selected-p buffer)
          (remhash buffer agent-shell-manager--done-unseen)
        (puthash buffer t agent-shell-manager--done-unseen))
      (when (agent-shell-manager--should-notify-ready-transition-p buffer)
        (agent-shell-manager--send-macos-notification
         "Agent Ready"
         (format "%s is ready"
                 (agent-shell-manager--notification-label buffer)))))))

(defun agent-shell-manager--prune-status-history (buffers)
  "Drop status cache entries for buffers not present in BUFFERS list."
  (maphash
   (lambda (buffer _status)
     (unless (memq buffer buffers)
       (remhash buffer agent-shell-manager--status-history)))
   agent-shell-manager--status-history))

(defun agent-shell-manager--prune-done-unseen (buffers)
  "Drop unseen completion entries for buffers not present in BUFFERS list."
  (maphash
   (lambda (buffer _done)
     (unless (memq buffer buffers)
       (remhash buffer agent-shell-manager--done-unseen)))
   agent-shell-manager--done-unseen))

(defun agent-shell-manager--poll-ready-transitions ()
  "Poll shell statuses and emit notifications for ready transitions."
  (let* ((buffers (agent-shell-buffers))
         (buffers (if (listp buffers) buffers (list buffers)))
         (buffers (seq-filter #'buffer-live-p buffers)))
    (agent-shell-manager--prune-status-history buffers)
    (agent-shell-manager--prune-done-unseen buffers)
    (dolist (buffer buffers)
      (let ((status (agent-shell-manager--get-status buffer)))
        (agent-shell-manager--maybe-notify-ready-transition buffer status)))))

(defun agent-shell-manager--ensure-notification-timer ()
  "Ensure status polling timer for notifications is running."
  (unless (and agent-shell-manager--notification-timer
               (timerp agent-shell-manager--notification-timer))
    (setq agent-shell-manager--notification-timer
          (run-with-timer 1 1 #'agent-shell-manager--poll-ready-transitions))))

(defun agent-shell-manager--stop-notification-timer ()
  "Stop status polling timer for notifications."
  (when (and agent-shell-manager--notification-timer
             (timerp agent-shell-manager--notification-timer))
    (cancel-timer agent-shell-manager--notification-timer)
    (setq agent-shell-manager--notification-timer nil)))

(defun agent-shell-manager--get-buffer-name (buffer)
  "Get the buffer name for BUFFER."
  (let ((name (buffer-name buffer)))
    (if (string-match " Agent @ \\(.*\\)\\'" name)
        (match-string 1 name)
      name)))

(defun agent-shell-manager--get-provider (buffer)
  "Get the provider name for BUFFER."
  (with-current-buffer buffer
    (let ((name (buffer-name)))
      (if (string-match "\\`\\(.*?\\) Agent @ " name)
          (match-string 1 name)
        "-"))))

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

(defun agent-shell-manager--get-combined-status (buffer &optional raw-status)
  "Get combined status for BUFFER that merges operational and session state.
Returns a user-friendly status string with appropriate face.
When RAW-STATUS is non-nil, use it instead of recomputing buffer status."
  (with-current-buffer buffer
    (let ((status (or raw-status (agent-shell-manager--get-status buffer)))
          (session (agent-shell-manager--get-session-status buffer)))
      (cond
       ;; Killed - highest priority
       ((string= status "killed")
        (propertize "Killed" 'face 'error))
       ;; Initializing without session
       ((and (string= status "initializing")
             (string= session "none"))
        (propertize "Starting..." 'face 'font-lock-comment-face))
       ;; Ready but no session (edge case)
       ((and (string= status "ready")
             (string= session "none"))
        (propertize "No Session" 'face 'font-lock-comment-face))
       ;; Ready with active session
       ((and (string= status "ready")
             (string= session "active"))
        (if (agent-shell-manager--done-unseen-p buffer)
            (propertize "Done" 'face 'agent-shell-manager-done)
          (propertize "Ready" 'face 'success)))
       ;; Working
       ((string= status "working")
        (propertize "Working" 'face 'warning))
       ;; Waiting for user input/permission
       ((string= status "waiting")
        (propertize "Waiting" 'face 'font-lock-keyword-face))
       ;; Unknown/fallback
       (t
        (propertize "Unknown" 'face 'font-lock-comment-face))))))

(defun agent-shell-manager--get-session-mode (buffer)
  "Get the current session mode for BUFFER."
  (with-current-buffer buffer
    (if (or agent-shell-manager--mode-id
            (and (boundp 'agent-shell--state)
                 (map-nested-elt agent-shell--state '(:session :mode-id))))
        (or (agent-shell--resolve-session-mode-name
             (or agent-shell-manager--mode-id
                 (map-nested-elt agent-shell--state '(:session :mode-id)))
             (and (boundp 'agent-shell--state)
                  (map-nested-elt agent-shell--state '(:session :modes))))
            agent-shell-manager--mode-id
            "-")
      "-")))

(defun agent-shell-manager--get-agent-kind (buffer)
  "Get the agent kind for BUFFER by parsing the buffer name."
  (with-current-buffer buffer
    (let ((buffer-name (buffer-name)))
      ;; Buffer names are in the format: "Agent Name Agent @ /path/to/dir"
      ;; Extract the agent name before " Agent @ "
      (if (string-match "^\\(.*?\\) Agent @ " buffer-name)
          (match-string 1 buffer-name)
        "-"))))

(defun agent-shell-manager--map-elt-any (map keys)
  "Return first non-nil value in MAP for KEYS."
  (seq-some (lambda (key)
              (map-elt map key))
            keys))

(defun agent-shell-manager--config-option-model-p (option)
  "Return non-nil when OPTION describes a model selector."
  (let ((category (agent-shell-manager--map-elt-any option '(:category category)))
        (id (agent-shell-manager--map-elt-any option '(:id id))))
    (or (equal category "model")
        (equal id "model"))))

(defun agent-shell-manager--model-config-option (config-options)
  "Return the model config option from CONFIG-OPTIONS."
  (seq-find #'agent-shell-manager--config-option-model-p config-options))

(defun agent-shell-manager--config-option-mode-p (option)
  "Return non-nil when OPTION describes a mode selector."
  (let ((category (agent-shell-manager--map-elt-any option '(:category category)))
        (id (agent-shell-manager--map-elt-any option '(:id id))))
    (or (equal category "mode")
        (equal id "mode"))))

(defun agent-shell-manager--mode-config-option (config-options)
  "Return the mode config option from CONFIG-OPTIONS."
  (seq-find #'agent-shell-manager--config-option-mode-p config-options))

(defun agent-shell-manager--config-option-current-value (option)
  "Return OPTION's current value."
  (agent-shell-manager--map-elt-any option
                                    '(:current-value current-value
                                      :currentValue currentValue)))

(defun agent-shell-manager--config-option-models (option)
  "Return `agent-shell' style model entries from OPTION values."
  (mapcar (lambda (value)
            `((:model-id . ,(agent-shell-manager--map-elt-any value '(:value value)))
              (:name . ,(agent-shell-manager--map-elt-any value '(:name name)))
              (:description . ,(agent-shell-manager--map-elt-any value
                                                              '(:description description)))))
          (agent-shell-manager--map-elt-any option '(:options options))))

(defun agent-shell-manager--config-option-modes (option)
  "Return `agent-shell' style mode entries from OPTION values."
  (mapcar (lambda (value)
            `((:id . ,(agent-shell-manager--map-elt-any value '(:value value)))
              (:name . ,(agent-shell-manager--map-elt-any value '(:name name)))
              (:description . ,(agent-shell-manager--map-elt-any value
                                                              '(:description description)))))
          (agent-shell-manager--map-elt-any option '(:options options))))

(defun agent-shell-manager--model-name (model-id models)
  "Return display name for MODEL-ID from MODELS, or nil."
  (map-elt (seq-find (lambda (model)
                       (string= (map-elt model :model-id) model-id))
                     models)
           :name))

(defun agent-shell-manager--current-model-id (state)
  "Return current model ID from STATE."
  (if (fboundp 'agent-shell--current-model-id)
      (agent-shell--current-model-id state)
    (map-nested-elt state '(:session :model-id))))

(defun agent-shell-manager--available-models (state)
  "Return available model entries from STATE."
  (if (fboundp 'agent-shell--get-available-models)
      (agent-shell--get-available-models state)
    (map-nested-elt state '(:session :models))))

(defun agent-shell-manager--apply-config-options (state config-options)
  "Update STATE from ACP CONFIG-OPTIONS.
Returns non-nil when a model or mode option was applied."
  (let* ((model-option
          (agent-shell-manager--model-config-option config-options))
         (model-id
          (and model-option
               (agent-shell-manager--config-option-current-value model-option)))
         (mode-option
          (agent-shell-manager--mode-config-option config-options))
         (mode-id
          (and mode-option
               (agent-shell-manager--config-option-current-value mode-option))))
    (when (or model-id mode-id)
      (let ((updated-session (map-elt state :session))
            (models (and model-option
                         (agent-shell-manager--config-option-models model-option)))
            (modes (and mode-option
                        (agent-shell-manager--config-option-modes mode-option))))
      (if updated-session
          (progn
            (when model-id
              (map-put! updated-session :model-id model-id)
              (map-put! updated-session :models models))
            (when mode-id
              (map-put! updated-session :mode-id mode-id)
              (map-put! updated-session :modes modes))
            (map-put! updated-session :config-options config-options))
        (setq updated-session
              `(,@(when model-id
                    `((:model-id . ,model-id)
                      (:models . ,models)))
                ,@(when mode-id
                    `((:mode-id . ,mode-id)
                      (:modes . ,modes)))
                (:config-options . ,config-options))))
      (map-put! state :session updated-session)
      (when-let* ((buffer (map-elt state :buffer)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (when model-id
              (setq-local agent-shell-manager--model-id model-id))
            (when mode-id
              (setq-local agent-shell-manager--mode-id mode-id)))))
      t))))

(defun agent-shell-manager--get-model-id (buffer)
  "Get the current model ID for BUFFER."
  (with-current-buffer buffer
    (let* ((state (and (boundp 'agent-shell--state) agent-shell--state))
           (state-model-id (and state
                                (agent-shell-manager--current-model-id state)))
           (model-id (or agent-shell-manager--model-id state-model-id))
           (models (and state
                        (agent-shell-manager--available-models state))))
      (if model-id
          (or (agent-shell-manager--model-name model-id models)
              model-id)
        "-"))))

(defun agent-shell-manager--count-pending-permissions (buffer)
  "Count the number of pending permission requests for BUFFER.
Returns a propertized string with yellow/warning face for non-zero counts."
  (with-current-buffer buffer
    (if (and (boundp 'agent-shell--state)
             (map-elt agent-shell--state :tool-calls))
        (let ((count 0))
          (map-do
           (lambda (_tool-call-id tool-call-data)
             (when (and (map-elt tool-call-data :permission-request-id)
                        (let ((status (map-elt tool-call-data :status)))
                          (equal status "pending")))
               (setq count (1+ count))))
           (map-elt agent-shell--state :tool-calls))
          (if (> count 0)
              (propertize (number-to-string count)
                          'face 'warning
                          'font-lock-face 'warning)
            "-"))
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

(defun agent-shell-manager--get-cwd (buffer)
  "Get the current session directory for BUFFER."
  (with-current-buffer buffer
    default-directory))

(defun agent-shell-manager--get-annotation (buffer)
  "Get annotation for BUFFER."
  (with-current-buffer buffer
    (or agent-shell-manager--annotation "-")))

(defun agent-shell-manager--annotation-target-buffer ()
  "Get the `agent-shell' buffer target for annotation commands."
  (cond
   ((derived-mode-p 'agent-shell-mode)
    (current-buffer))
   ((derived-mode-p 'agent-shell-manager-mode)
    (or (tabulated-list-get-id)
        (user-error "No agent-shell buffer at point")))
   (t
    (user-error "Run this in an agent-shell buffer or the manager"))))

(defun agent-shell-manager-set-annotation ()
  "Set annotation for an `agent-shell' buffer.

When called from an `agent-shell' buffer, edits its own annotation.
When called from the manager, edits the annotation for the buffer at point.
Submit empty input to clear the current annotation."
  (interactive)
  (let* ((buffer (agent-shell-manager--annotation-target-buffer))
         (current (with-current-buffer buffer
                    (or agent-shell-manager--annotation "")))
         (raw-input (read-string
                     (format "Annotation for %s (empty to clear): "
                             (buffer-name buffer))
                     current))
         (annotation (string-trim raw-input)))
    (with-current-buffer buffer
      (setq-local agent-shell-manager--annotation
                  (unless (string= annotation "") annotation)))
    (agent-shell-manager-refresh)
    (message "%s annotation for %s"
             (if (string= annotation "") "Cleared" "Updated")
             (buffer-name buffer))))

(defun agent-shell-manager--valid-command-names (buffer)
  "Return valid slash command names for BUFFER.

Values come from BUFFER's `:available-commands' entries and do not
include the leading slash."
  (with-current-buffer buffer
    (when (boundp 'agent-shell--state)
      (mapcar (lambda (command)
                (map-elt command 'name))
              (map-elt agent-shell--state :available-commands)))))

(defun agent-shell-manager--extract-command-name (input)
  "Return slash command name parsed from INPUT, or nil."
  (when (and (stringp input)
             (string-match "^\\s-*/\\s-*\\([^[:space:]]+\\)" input))
    (match-string 1 input)))

(defun agent-shell-manager--extract-model-id (input)
  "Return model ID parsed from a slash model INPUT, or nil."
  (when (and (stringp input)
             (string-match "^\\s-*/\\s-*model\\s-+\\([^[:space:]]+\\)" input))
    (match-string 1 input)))

(defun agent-shell-manager--extract-mode-id (input)
  "Return mode ID parsed from a slash mode INPUT, or nil."
  (when (and (stringp input)
             (string-match "^\\s-*/\\s-*mode\\s-+\\([^[:space:]]+\\)" input))
    (match-string 1 input)))

(defun agent-shell-manager--track-last-command (input)
  "Track the last valid slash command submitted from the current buffer.

INPUT is the raw command text received from `comint-input-filter-functions'."
  (let* ((command-name (agent-shell-manager--extract-command-name input))
         (valid-commands (agent-shell-manager--valid-command-names (current-buffer))))
    (when (and command-name
               (or (null valid-commands)
                   (member command-name valid-commands)))
      (setq-local agent-shell-manager--last-command
                  (format "/%s" command-name)))
    (when-let* ((model-id (agent-shell-manager--extract-model-id input)))
      (setq-local agent-shell-manager--model-id model-id)
      (agent-shell-manager-refresh))
    (when-let* ((mode-id (agent-shell-manager--extract-mode-id input)))
      (setq-local agent-shell-manager--mode-id mode-id)
      (agent-shell-manager-refresh))))

(defun agent-shell-manager--ensure-command-tracker (&optional buffer)
  "Install per-buffer command tracking hook for BUFFER.

When BUFFER is nil, uses the current buffer."
  (let ((target-buffer (or buffer (current-buffer))))
    (when (buffer-live-p target-buffer)
      (with-current-buffer target-buffer
        (when (derived-mode-p 'agent-shell-mode)
          (add-hook 'comint-input-filter-functions
                    #'agent-shell-manager--track-last-command
                    nil t))))))

(defun agent-shell-manager--ensure-command-trackers ()
  "Install command tracking hooks in all live `agent-shell' buffers."
  (let* ((buffers (agent-shell-buffers))
         (buffers (if (listp buffers) buffers (list buffers))))
    (mapc #'agent-shell-manager--ensure-command-tracker
          (seq-filter #'buffer-live-p buffers))))

(defun agent-shell-manager--get-last-command (buffer)
  "Get the last valid slash command used in BUFFER.

Returns \"-\" if no valid slash command has been submitted yet in BUFFER."
  (with-current-buffer buffer
    (or agent-shell-manager--last-command "-")))

(defun agent-shell-manager--column-value (column buffer raw-status)
  "Return BUFFER value for COLUMN.

RAW-STATUS is the precomputed operational status for BUFFER."
  (pcase column
    ('buffer (agent-shell-manager--get-buffer-name buffer))
    ('provider (agent-shell-manager--get-provider buffer))
    ('status (agent-shell-manager--get-combined-status buffer raw-status))
    ('mode (agent-shell-manager--get-session-mode buffer))
    ('model (agent-shell-manager--get-model-id buffer))
    ('pending-permissions (agent-shell-manager--count-pending-permissions buffer))
    ('annotation (agent-shell-manager--get-annotation buffer))
    ('path (abbreviate-file-name (agent-shell-manager--get-cwd buffer)))
    ('last-command (agent-shell-manager--get-last-command buffer))
    (_ "-")))

(defun agent-shell-manager--formatted-column-value (column buffer raw-status)
  "Return formatted value for COLUMN in BUFFER.

RAW-STATUS is the precomputed operational status for BUFFER."
  (agent-shell-manager--truncate-cell
   (agent-shell-manager--column-value column buffer raw-status)
   (agent-shell-manager--column-width
    column agent-shell-manager--column-widths)))

(defun agent-shell-manager--entries ()
  "Return list of entries for tabulated-list."
  (let* ((columns (agent-shell-manager--visible-columns))
         (buffers (agent-shell-buffers))
         (buffers (if (listp buffers) buffers (list buffers)))
         (buffers (seq-filter #'buffer-live-p buffers))
         (_ignored (agent-shell-manager--prune-status-history buffers))
         (_ignored-done (agent-shell-manager--prune-done-unseen buffers))
         (_ignored-command-trackers
          (mapc #'agent-shell-manager--ensure-command-tracker buffers))
         (entries (mapcar
                   (lambda (buffer)
                     (let* ((raw-status (agent-shell-manager--get-status buffer))
                            (_notify (agent-shell-manager--maybe-notify-ready-transition
                                      buffer raw-status)))
                       (list buffer
                             (vconcat
                              (mapcar (lambda (column)
                                        (agent-shell-manager--formatted-column-value
                                         column buffer raw-status))
                                      columns)))))
                   buffers)))
    ;; Sort entries: killed processes go to the bottom
    (sort entries
          (lambda (a b)
            (let ((killed-a (string= (agent-shell-manager--get-status (car a)) "killed"))
                  (killed-b (string= (agent-shell-manager--get-status (car b)) "killed")))
              (cond
               ;; Both killed or both not killed - maintain original order (stable)
               ((eq killed-a killed-b) nil)
               ;; a is killed, b is not - a goes after b
               (killed-a nil)
               ;; b is killed, a is not - a goes before b
               (t t)))))))

(defun agent-shell-manager-refresh ()
  "Refresh the buffer list."
  (interactive)
  (when (and agent-shell-manager--global-buffer
             (buffer-live-p agent-shell-manager--global-buffer))
    (with-current-buffer agent-shell-manager--global-buffer
      (agent-shell-manager--apply-column-configuration
       agent-shell-manager--column-widths)
      (setq tabulated-list-entries (agent-shell-manager--entries))
      (tabulated-list-print t))))

(defun agent-shell-manager--refresh-visible-manager (&rest _args)
  "Refresh the manager when it is visible."
  (when (agent-shell-manager--manager-visible-p)
    (agent-shell-manager-refresh)))

(defun agent-shell-manager--after-agent-shell-notification (&rest args)
  "Refresh model state after `agent-shell--on-notification' handles ARGS."
  (let ((state (plist-get args :state))
        (acp-notification (plist-get args :acp-notification)))
    (when state
      (pcase (map-nested-elt acp-notification '(params update sessionUpdate))
        ("config_option_update"
         (agent-shell-manager-refresh))
        ("current_mode_update"
         (when-let* ((mode-id (or (map-nested-elt acp-notification
                                                  '(params update currentModeId))
                                  (map-nested-elt acp-notification
                                                  '(params update modeId)))))
           (when-let* ((updated-session (map-elt state :session)))
             (map-put! updated-session :mode-id mode-id)
             (map-put! state :session updated-session))
           (when-let* ((buffer (map-elt state :buffer)))
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (setq-local agent-shell-manager--mode-id mode-id)
                 (agent-shell--update-header-and-mode-line))))
           (agent-shell-manager-refresh)))))))

(defun agent-shell-manager--hide-window ()
  "Hide the manager window if `agent-shell-manager-transient' is non-nil."
  (when agent-shell-manager-transient
    (when-let* ((buffer agent-shell-manager--global-buffer)
                (window (and (buffer-live-p buffer)
                             (get-buffer-window buffer))))
      (delete-window window))))

(defun agent-shell-manager-goto ()
  "Go to the `agent-shell' buffer at point.
If `agent-shell-manager-transient' is non-nil, hide the manager window.
If the buffer is already visible, switch to it.
Otherwise, if another `agent-shell' window is open, reuse it."
  (interactive)
  (when-let* ((buffer (tabulated-list-get-id)))
    (if (buffer-live-p buffer)
        (let ((buffer-window (get-buffer-window buffer t))
              (agent-shell-window nil))
          (cond
           ;; If the buffer is already visible, just switch to it
           (buffer-window
            (select-window buffer-window))

           ;; Otherwise, find an existing agent-shell window to reuse
           (t
            (walk-windows
             (lambda (win)
               (when (and (not agent-shell-window)
                          (not (eq win (selected-window)))
                          (with-current-buffer (window-buffer win)
                            (derived-mode-p 'agent-shell-mode)))
                 (setq agent-shell-window win)))
             nil t)

            (if agent-shell-window
                ;; Reuse the existing agent-shell window
                (progn
                  (set-window-buffer agent-shell-window buffer)
                  (select-window agent-shell-window))
              ;; No existing agent-shell window, use default behavior
              (agent-shell--display-buffer buffer))))
          (agent-shell-manager--clear-done-unseen buffer)
          (agent-shell-manager--hide-window))
      (user-error "Buffer no longer exists"))))

(defun agent-shell-manager-kill ()
  "Kill the `agent-shell' process at point."
  (interactive)
  (when-let* ((buffer (tabulated-list-get-id)))
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
  "Create a new `agent-shell'."
  (interactive)
  (agent-shell t)
  (if agent-shell-manager-transient
      (agent-shell-manager--hide-window)
    (agent-shell-manager-refresh)))

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
  "Restart the `agent-shell' at point.
Kills the current process and starts a new one with the same config if possible."
  (interactive)
  (when-let* ((buffer (tabulated-list-get-id)))
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
  "Delete all killed `agent-shell' buffers from the list."
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
  "Set session mode for the `agent-shell' at point."
  (interactive)
  (when-let* ((buffer (tabulated-list-get-id)))
    (unless (buffer-live-p buffer)
      (user-error "Buffer no longer exists"))
    (with-current-buffer buffer
      (unless (derived-mode-p 'agent-shell-mode)
        (user-error "Not an agent-shell buffer"))
      (agent-shell-set-session-mode #'agent-shell-manager-refresh))))

(defun agent-shell-manager-set-model ()
  "Set session model for the `agent-shell' at point."
  (interactive)
  (when-let* ((buffer (tabulated-list-get-id)))
    (unless (buffer-live-p buffer)
      (user-error "Buffer no longer exists"))
    (with-current-buffer buffer
      (unless (derived-mode-p 'agent-shell-mode)
        (user-error "Not an agent-shell buffer"))
      (agent-shell-set-session-model #'agent-shell-manager-refresh))))

(defun agent-shell-manager-interrupt ()
  "Interrupt the `agent-shell' at point."
  (interactive)
  (when-let* ((buffer (tabulated-list-get-id)))
    (unless (buffer-live-p buffer)
      (user-error "Buffer no longer exists"))
    (with-current-buffer buffer
      (unless (derived-mode-p 'agent-shell-mode)
        (user-error "Not an agent-shell buffer"))
      (agent-shell-interrupt))
    (agent-shell-manager-refresh)))

(defun agent-shell-manager-view-traffic ()
  "View traffic logs for the `agent-shell' at point."
  (interactive)
  (when-let* ((buffer (tabulated-list-get-id)))
    (unless (buffer-live-p buffer)
      (user-error "Buffer no longer exists"))
    (with-current-buffer buffer
      (unless (derived-mode-p 'agent-shell-mode)
        (user-error "Not an agent-shell buffer"))
      (agent-shell-view-traffic))))

(defun agent-shell-manager-toggle-logging ()
  "Toggle logging for `agent-shell'."
  (interactive)
  (agent-shell-toggle-logging)
  (agent-shell-manager-refresh))

(defun agent-shell-manager-toggle-ready-status-notifications ()
  "Toggle `agent-shell-manager-ready-status-notifications'."
  (interactive)
  (setq agent-shell-manager-ready-status-notifications
        (not agent-shell-manager-ready-status-notifications))
  (if agent-shell-manager-ready-status-notifications
      (agent-shell-manager--ensure-notification-timer)
    (agent-shell-manager--stop-notification-timer))
  (message "Ready status notifications %s"
           (if agent-shell-manager-ready-status-notifications
               "enabled"
             "disabled")))

(defun agent-shell-manager-toggle-ready-status-notification-sound ()
  "Toggle `agent-shell-manager-ready-status-notification-sound'."
  (interactive)
  (setq agent-shell-manager-ready-status-notification-sound
        (not agent-shell-manager-ready-status-notification-sound))
  (message "Ready status notification sound %s"
           (if agent-shell-manager-ready-status-notification-sound
               "enabled"
             "disabled")))

(defun agent-shell-manager--clamp (value minimum maximum)
  "Clamp VALUE between MINIMUM and MAXIMUM."
  (min maximum (max minimum value)))

(defun agent-shell-manager--side-window-size (side size)
  "Return window SIZE for SIDE.

Left and right side windows are clamped between
`agent-shell-manager-side-window-min-width' and
`agent-shell-manager-side-window-max-width'."
  (if (memq side '(left right))
      (agent-shell-manager--clamp
       (if (floatp size)
           (floor (* (frame-width) size))
         size)
       agent-shell-manager-side-window-min-width
       agent-shell-manager-side-window-max-width)
    size))

(defun agent-shell-manager--resize-side-window (window side size)
  "Resize WINDOW at SIDE to SIZE when it is a left/right side window."
  (when (and (window-live-p window)
             (memq side '(left right)))
    (let* ((target-width (agent-shell-manager--side-window-size side size))
           (delta (- target-width (window-total-width window))))
      (unless (zerop delta)
        (ignore-errors
          (window-resize window delta t t))))))

(defun agent-shell-manager--display-buffer-in-side-window (buffer side size)
  "Display BUFFER in a side window at SIDE using SIZE."
  (let ((size-param (if (memq side '(left right))
                        'window-width
                      'window-height))
        (window-size (agent-shell-manager--side-window-size side size)))
    (let ((window
           (display-buffer-in-side-window
            buffer
            `((side . ,side)
              (slot . 0)
              (,size-param . ,window-size)
              (preserve-size . ,(if (memq side '(left right))
                                    '(t . nil)
                                  '(nil . t)))
              ,@(unless agent-shell-manager-transient
                  '((window-parameters .
                     ((no-delete-other-windows . t)))))))))
      (agent-shell-manager--resize-side-window window side size)
      window)))

(defun agent-shell-manager--show-buffer (&optional side size column-widths padding)
  "Show the manager buffer.

When SIDE is non-nil, display it in a side window using SIZE.
Otherwise, use `agent-shell-manager-side' or the user's
`display-buffer' configuration."
  (let* ((buffer (get-buffer-create "*Agent-Shell Buffers*"))
         (old-window (get-buffer-window buffer)))
    (when (window-live-p old-window)
      (set-window-dedicated-p old-window nil)
      (when (window-deletable-p old-window)
        (delete-window old-window)))
    (let ((window (cond
                   (side
                    (agent-shell-manager--display-buffer-in-side-window
                     buffer side size))
                   (agent-shell-manager-side
                    (agent-shell-manager--display-buffer-in-side-window
                     buffer agent-shell-manager-side 0.3))
                   (t
                    (display-buffer buffer)))))
      (setq agent-shell-manager--global-buffer buffer)
      (with-current-buffer buffer
        (agent-shell-manager-mode)
        (setq-local agent-shell-manager--column-widths column-widths)
        (when padding
          (setq-local tabulated-list-padding padding))
        (agent-shell-manager-refresh))
      (set-window-dedicated-p window t)
      (select-window window))))

(defun agent-shell-manager-switch-to-side-window ()
  "Move manager to a compact side window."
  (interactive)
  (unless (equal agent-shell-manager-visible-columns
                 agent-shell-manager-side-window-columns)
    (setq agent-shell-manager--default-visible-columns
          agent-shell-manager-visible-columns))
  (setq agent-shell-manager-visible-columns
        agent-shell-manager-side-window-columns)
  (agent-shell-manager--show-buffer
   agent-shell-manager-side-window-side
   agent-shell-manager-side-window-width
   agent-shell-manager-side-window-column-widths
   1))

(defun agent-shell-manager-switch-to-default-window ()
  "Move manager to its default window and restore previous columns."
  (interactive)
  (when agent-shell-manager--default-visible-columns
    (setq agent-shell-manager-visible-columns
          agent-shell-manager--default-visible-columns))
  (agent-shell-manager--show-buffer nil nil nil 2))

;;;###autoload
(defun agent-shell-manager-toggle ()
  "Toggle the `agent-shell' buffer list window.
Shows agent buffers in a configurable tabulated list.
The position of the window is controlled by `agent-shell-manager-side'.
When `agent-shell-manager-transient' is non-nil, the window can be closed
by `delete-other-windows' (C-x 1)."
  (interactive)
  (let* ((buffer (get-buffer-create "*Agent-Shell Buffers*"))
         (window (get-buffer-window buffer)))
    (if (and window (window-live-p window))
        ;; Window is visible, hide it
        (delete-window window)
      ;; Window is not visible, show it.
      (agent-shell-manager--show-buffer))))

(add-hook 'agent-shell-mode-hook #'agent-shell-manager--ensure-command-tracker)
(agent-shell-manager--ensure-command-trackers)

(unless (advice-member-p #'agent-shell-manager--refresh-visible-manager
                         'agent-shell--update-header-and-mode-line)
  (advice-add 'agent-shell--update-header-and-mode-line
              :after #'agent-shell-manager--refresh-visible-manager))

(unless (advice-member-p #'agent-shell-manager--after-agent-shell-notification
                         'agent-shell--on-notification)
  (advice-add 'agent-shell--on-notification
              :after #'agent-shell-manager--after-agent-shell-notification))

(add-hook 'post-command-hook #'agent-shell-manager--mark-current-agent-visited)

(when agent-shell-manager-ready-status-notifications
  (agent-shell-manager--ensure-notification-timer))

(provide 'agent-shell-manager)

;;; agent-shell-manager.el ends here
