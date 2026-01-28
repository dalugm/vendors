(progn
  (ql:quickload '(#:dexador #:com.inuoe.jzon))
  (uiop:add-package-local-nickname :dex :dexador)
  (uiop:add-package-local-nickname :jzon :com.inuoe.jzon))

(defun download-latest-release (owner repo &key (output-dir "./"))
  (let* ((uri (format nil
                      "https://api.github.com/repos/~A/~A/releases/latest"
                      owner
                      repo))
         (headers  '(("User-Agent" . "cl-asset-downloader")))
         (response (dex:get uri :headers headers))
         (release  (jzon:parse response))
         (assets   (gethash "assets" release)))
    (if (zerop (length assets))
        (format t "No assets found in the latest release of ~A/~A~%" owner repo)
        (let ((output-dir (uiop:ensure-directory-pathname output-dir)))
          (ensure-directories-exist output-dir)
          (format t "Found ~D asset(s)~%" (length assets))
          (loop for asset across assets do
            (let* ((filename     (gethash "name" asset))
                   (download-uri (gethash "browser_download_url" asset))
                   (output-path
                     (merge-pathnames filename (pathname output-dir))))
              (format t "Downloading ~A ... " filename)
              (force-output)
              ;; (with-open-file (out output-path
              ;;                      :direction :output
              ;;                      :element-type '(unsigned-byte 8)
              ;;                      :if-exists :supersede
              ;;                      :if-does-not-exist :create)
              ;;   (write-sequence (dexador:get download-uri) out))
              (dex:fetch download-uri output-path :if-exists :supersede)
              (format t "done.~%")))
          (format t
                  "~&~D asset(s) saved to ~A.~%"
                  (length assets)
                  output-dir)))))

#|
(download-latest-release "Loyalsoldier"
                         "v2ray-rules-dat"
                         :output-dir "~/Downloads/geo")
|#
