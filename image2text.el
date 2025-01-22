;;; image2text.el --- Converts images to propertized text using `ascii-image-converter' -*- lexical-binding: t -*-

;; Copyright (C) 2025 blu3

;;; License:

;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'seq)
(require 'ht)
(require 'ansi-color)
(require 'shr)
;; Use text properties instead of overlays
(setq ansi-color-apply-face-function #'ansi-color-apply-text-property-face)

(defvar image2text--bin "ascii-image-converter"
  "Path to the `ascii-image-converter` binary used for image-to-text conversion.")

(defvar image2text--buffer-name "*image2text*"
  "Default buffer name for storing temporary output.")

;; Allows for colored text in GUI Emacs.
(defvar image2text--env "TERM=xterm-24bits"
  "Environment to execute the binary with.")

(defvar image2text-cache-size 128 "Size of image cache.")

(defvar image2text--cache
  (make-hash-table :size image2text-cache-size :test #'eq)
  "Text image cache.")

;;; Functions:


(defun image2text-clear-cache (&optional no-gc)
  "Clear image cache and garbage collect."
  (interactive "yInvoke GC?")
  (ht-clear image2text--cache)
  (when no-gc
    (garbage-collect)))


(defun hash-image-data (data)
  "Hash image `DATA'."
  (sxhash data))

(defun hash-current-buffer-string ()
  "Hash image `DATA'."

  ;; Switch to `BUFFER' if given.
  (sxhash (buffer-string)))

(defun image2text-cache-lookup (image-id)
  "Lookup `IMAGE-ID' in cache, if it exists, return the image text.

Returns nil if no image was found in cache."
  (message "[cache] Looking for '%S'" image-id)
  (if (ht-contains-p image2text--cache image-id)
      (ht-get image2text--cache image-id)
    nil))

(defun image2text-cache-put (image-id text-image)
  "Cache `TEXT-IMAGE' identified by `IMAGE-ID'.

Returns nil if image already exists in cache."
  (message "[cache] In '%S' storing '...''" image-id)
  (unless (ht-contains-p image2text--cache image-id)
    (ht-set! image2text--cache image-id text-image))
  text-image)


(defun image2text--validate-options (threshold)
  "Validate options for image2text.
Checks that THRESHOLD is within the valid range (0–255)."
  (when (and threshold (or (< threshold 0) (> threshold 255)))
    (error "Threshold must be between 0 and 255")))

(defun image2text--check-binary ()
  "Check if the `ascii-image-converter` binary exists."
  (unless (executable-find image2text--bin)
    (error "The binary `%s` is not found in PATH" image2text--bin)))

(cl-defun image2text
    (path &key
          data
          kill-buffer
          color
          negative
          grayscale
          dither
					save-to-file
          complex
          braille
          url
          threshold
          width
          height)
  "Convert the image at PATH to text and return the result.
Options:
  :data         - Image data
  :url          - Non-nil if path is a url
  :save-to-file - Save output to file, will not save colors.
  :kill-buffer - Do not display the ASCII text in a buffer, only return the text string.
  :color       - Use colors in the ASCII art. Overrides :grayscale if both are set.
  :complex     - Use a larger range of characters for higher quality.
  :negative    - Invert the colors in the ASCII art.
  :grayscale   - Convert the image to grayscale before rendering.
  :braille     - Output the ASCII art using braille characters.
  :dither      - Apply dithering (only with :braille).
  :threshold   - Set the threshold for braille art (0–255).
  :width       - Specify the width of the output text.
  :height      - Specify the height of the output text."

  (image2text--validate-options threshold)
  (image2text--check-binary)

  (catch 'finished
    (let* ((data-hash ""))

      (let* ((temp-image-path nil))

        ;; DATA

        (when data
          ;; Hash data and look for a existing cached entry
          ;; before creating the temporary files.
          (setq data-hash (hash-image-data data))
          ;; Look in cache
          (let ((maybe-text (image2text-cache-lookup data-hash)))
            (when maybe-text (throw 'finished maybe-text)))

          (setq path (image2text--write-image-data data)))

        ;; URL

        (when (or url (url-p path))
          ;; Hash url
          (setq data-hash (hash-image-data url))
          ;; Look in cache
          (let ((maybe-text (image2text-cache-lookup data-hash)))
            (when maybe-text (throw 'finished maybe-text)))

          (message "Path: %s" path)
          (setq path (image2text--download-file path))
          (message "Path: %s" path)
          (shell-command (concat "magick -depth 24 " path " " path))
          (message "File downloaded!")
          (setq temp-image-path path))

        ;; Hash path
        (when (and path (not (string= path "")))
          (setq data-hash (hash-image-data path)))
        ;; Look in cache
        (let ((maybe-text (image2text-cache-lookup data-hash)))
          (when maybe-text (throw 'finished maybe-text)))


        (let ((output "")
              (bin image2text--bin)
              (env image2text--env)
              (args (mapcar (lambda (s) (format "%s" s))
                            (flatten-list
                             (list (and color           "--color")
											             (and negative        "--negative")
											             (and grayscale       "--grayscale")
											             (and complex         "--complex")
											             (and width (list     "--width"     width))
											             (and height (list    "--height"    height))
											             (and threshold (list "--threshold" threshold))
											             (and braille         "--braille")
											             (and dither          "--dither")))))

              ;; Temporarily increase GC threshold
              (gc-cons-threshold (expt 2 32)))

          (let ((cmd-str (concat env " " bin " " path " " (string-join args " "))))

          (unwind-protect
              (save-excursion
                ;; Run the command and store output in a temporary buffer
                (with-temp-buffer
                  ;; Execute command and insert in current buffer
                  (shell-command cmd-str t)

                  (replace-string "Fetching file from url...                          " "")
                  ;; Convert ANSI codes in output to text properties.
                  (ansi-color-apply-on-region (point-min) (point-max))
						      ;; Save text to file.
						      (when save-to-file
							      (write-region (point) (point-max) (expand-file-name save-to-file)))

                  (setq output (buffer-string)))

                (when (not kill-buffer)
                  (switch-to-buffer "*image2text*")
                  (erase-buffer)
                  (insert output))


                ;; Delete temporary file if one was created
                (and temp-image-path
                     (progn (message "Deleting temporary file!")
                            (delete-file temp-image-path))))))

          (image2text-cache-put data-hash output))))))

(provide 'image2text)


(defun overlay-to-text-property (start end)
  (mapc
   (lambda (ol)
     (let ((props (overlay-properties ol))
           (s (overlay-start ol))
           (e (overlay-end ol)))
       (delete-overlay ol)
       (while props
         (put-text-property s
                            e
                            (pop props)
                            (pop props)))))
   (overlays-in start end)))


;;; Auxillary functions:

(defvar image-type-extension-map
  '((png . ".png")
    (gif . ".gif")
    (jpeg . ".jpeg")
    (webp . ".webp")
    (bmp . ".bmp")
    (xpm . ".xpm")
    (pbm . ".pbm")
    (xbm . ".xbm")
    (postscript . ".ps")
    (tiff . ".tiff")
    (svg . ".svg")
    (heic . ".heic"))
  "Map of image types to their extension.")


(defun image2text--write-image-data (data &optional path)
  "Save the image under point.
This writes the original image data to a file.  Rotating or
changing the displayed image size does not affect the saved image."

  (let* ((path (or path (image2text--make-tmp-file)))
         (type nil))
    (interactive)
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert data)
      (setq type (image-type-from-buffer))

      ;; Append extension to path
      (setq path (concat path (or (alist-get type image-type-extension-map) "")))
      (message "Writing data to %S" path)
      (write-region (point-min) (point-max) path))

    ;; Convert gifs to png
    (if (eq type 'gif)
        (progn
          (shell-command
           (concat "magick " path "[0] "   "PNG24:" (file-name-base path) ".png"
                   " && " "rm " path))
          (concat (file-name-base path) ".png"))
      path)))

(defun image2text--image-to-file (image &optional filename path)
  "Write an `IMAGE' to a file."
  (with-temp-file (setq path (image2text--make-tmp-file filename))
    (insert image)))


(defun image2text--make-tmp-file (&optional url)
  "Create a temporary file name for `URL'."
  (let* ((ext (or (url-file-extension url) ""))
				 (file (concat (make-temp-file "image2text") ext)))
    file))


(defun image2text--download-file (url &optional path)
  "Download file at `URL' and return downloaded file path.

If `PATH' is non-nil, download `URL' to `PATH', otherwise
generate a temporary filename."
	(let* ((file (or path (make-file-name url)))
         (type nil))
    (message "Downloading %s to %s" url file)

		(with-temp-file file
      (set-buffer-multibyte nil)
			(insert-buffer (url-retrieve-synchronously url))
      (setq type (image-type-from-buffer))
			(search-forward "\n\n")
			(kill-region 1 (point))
      (setq file
            (concat file
                    (or (alist-get (image-type-from-buffer)
                                   image-type-extension-map)
                        ""))))

		file))

(provide 'image2text--download-file)



;;; SHR Stuff

(provide 'reset-shr-put-image-function)
(provide 'enable-shr-text-images)

(provide 'original-shr-put-image-function)
(provide 'image2text-shr-default-width)
(provide 'image2text-default-shr-props)

(provide 'shr-put-text-image)

;; SHR prompts for each image unless this is set.
(setq coding-system-for-write 'raw-text)

(defvar image2text-shr-default-width 30
  "Default width of text images")

(defvar original-shr-put-image-function shr-put-image-function
  "The original (non text) `shr-put-image-function'.")

(defvar image2text-default-shr-props
  (list
   :braille t

   :dither t
   :threshold 100
   :complex t
   :kill-buffer t
)
  "Default properties for the call to \\[image2text] in \\[shr-put-text-image].")

(defvar image2text--debug nil "Use debugging")

(defvar image2text-update-font-lock-after-insertion nil
  "If non-nil, call \\[font-lock-update] after each text insertion.
This may be nessisary if colors are enabled.")

(defun shr-put-text-image (spec alt &optional flags)
  "Custom `shr-put-image-function` that inserts a text representation of the image.
SPEC is the image specification, ALT is the alternative text, and FLAGS are optional."

  ;; Inhibit gc during conversion / lookup

  (let ((gc-cons-threshold (expt 2 32))
        (inhibit-message (not image2text--debug)))

    (let* ((image-data (plist-get spec :data))
           (image-width  (or (plist-get spec :width) image2text-shr-default-width))
           (image-type (plist-get spec :type))
           (image-height (plist-get spec :height)))

      (let ((actual-width
             (min shr-max-width
                  (or image-width image2text-shr-default-width))))
        (save-window-excursion
          (insert
           ;; Try cache before converting image data.
           (or (image2text-cache-lookup (hash-image-data (car spec)))
               (apply #'image2text
                      `("" :data ,(car spec)
                           :width ,actual-width
                        ,@image2text-default-shr-props))))
          (when image2text-update-font-lock-after-insertion
            (font-lock-update)))))))


(defun enable-shr-text-images nil
  "Enable using text images in SHR."
  (interactive)
  (setq shr-put-image-function #'shr-put-text-image)

  ;; Fontify at the end instead of after each image insertion
  (unless image2text-update-font-lock-after-insertion
    (add-hook 'eww-after-render-hook
              #'(lambda ()
                  (setq-local undo-outer-limit t)
                  (font-lock-fontify-buffer)))))

(defun reset-shr-put-image-function nil
  "Restore the original `shr-put-image-function`."
  (interactive)
  (setq shr-put-image-function original-shr-put-image-function))


(provide 'image2text)
;;; image2text.el ends here
