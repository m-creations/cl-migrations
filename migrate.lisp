;; CL-MIGRATIONS is released under the MIT Licence: 
;;
;; Copyright (c) <2006> <Vamsee Krishna Kanakala>
;;
;; Permission is hereby granted, free of charge, to any person 
;; obtaining a copy of this software and associated documentation 
;; files (the "Software"), to deal in the Software without restriction, 
;; including without limitation the rights to use, copy, modify, merge,
;; publish, distribute, sublicense, and/or sell copies of the Software,
;; and to permit persons to whom the Software is furnished to do so, 
;; subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, 
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND 
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT 
;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, 
;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, 
;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(in-package :cl-migrations)

(defun get-db-version ()
  (parse-integer
   (first (select 'version
            :from *schema-table-name*
            :flatp t
            :field-names nil))))

#.(locally-enable-sql-reader-syntax)
(defun incf-db-version (db-version mig-version)
  (let ((db-version/string (princ-to-string db-version))
        (mig-version/string (princ-to-string mig-version)))
    (update-records (make-symbol *schema-table-name*)
                    :av-pairs `((version ,mig-version/string))
                    :where [= [version] db-version/string])))

(defun decf-db-version (db-version mig-version)
  (let ((db-version/string (princ-to-string db-version))
        (mig-version/string (princ-to-string mig-version)))
    (update-records (make-symbol *schema-table-name*)
                    :av-pairs `((version ,mig-version/string))
                    :where [= [version] db-version/string])))
#.(restore-sql-reader-syntax-state)

(defun get-migration-number (file)
  (parse-integer 
   (subseq (file-namestring file) 0 (search "-" (file-namestring file))) :junk-allowed t))

(defun compare-files (file1 file2)
   (< (get-migration-number file1) (get-migration-number file2)))

(defun get-migration-files ()
  "Get all files from the migrations directory, and sort them numerically."
  (unless *migration-dir*
    (warn "Migrations directory not set properly. Please check your config file.")
    (return-from get-migration-files :skipped))
  (let ((files (directory (make-pathname :name :wild
					 :type "lisp"
					 :defaults *migration-dir*))))
    (sort (remove-if (lambda(x) (null (get-migration-number x))) files)
	  #'compare-files)))

(defun get-latest-migration ()
  "Get the version of latest migration available."
  (let ((files-list (get-migration-files)))
    (unless (eql files-list :skipped)
      (get-migration-number (first (last files-list))))))

(defun current-date-string ()
  "Generates the current date as string of format YYYYMMDDhhmmss."
  (let ((date (reverse (subseq (multiple-value-list (get-decoded-time)) 0 6))))
    (format nil "~{~2,'0D~}" date)))

(defun generate (name)
  "Generate an empty migration file with an assigned version number."
  (unless *migration-dir*
    (read-specs))
  (let* ((file-name (make-pathname 
                     :name (concatenate 
                            'string 
                            (current-date-string) "-" name)
                     :type "lisp"
                     :defaults *migration-dir*))
         (file-name-str (file-namestring file-name)))
    (ensure-directories-exist file-name)
    (with-open-file (stream file-name 
                            :direction :output 
                            :if-does-not-exist :create)
      (write-line (concatenate 'string ";; " file-name-str) stream)
      (with-standard-io-syntax 
        (prin1 '(:up () :down ()) stream)))
    file-name-str))
  
(defun get-file-range (from to)
  "Get the migration files within the given range, in ascending order."
  (remove-if #'(lambda (file)
		 (or (< (get-migration-number file) from)
		     (> (get-migration-number file) to))) (get-migration-files)))
  
(defun select-migration-files (db-version mig-version)
  "Return migration files in ascending or descending order based on 
   whether migrating up or down respectively."
    (if (> mig-version db-version)
	(get-file-range (1+ db-version) mig-version)
	(reverse (get-file-range (1+ mig-version) db-version))))

(defun exec-migrations (db-version mig-version)
  "Excute all the migrations within the given range in a transaction."
  (let ((files (select-migration-files db-version mig-version)))
    (with-transaction ()
      (dolist (file files (length files))
	(format t "~%~%Migration #~S:" (get-migration-number file))
	(with-open-file (stream file)
	  (with-standard-io-syntax
	    (let* ((migration (read stream))
		   (ddl (if (> mig-version db-version)
			    (getf migration :up)
                            (getf migration :down))))
	      (dolist (statement ddl)
		(format t "~%EXEC: ~S" (first statement))
		(execute-command (first statement))))))
	(if (> mig-version db-version)
	    (incf-db-version db-version mig-version)
	    (decf-db-version db-version mig-version))))))
  
(defun migrate (&key version)
  "Initiate migration procedure."
  (when (init-config)
    (unwind-protect
	 (let ((db-version (get-db-version))
	       (mig-version (get-latest-migration)))
	   (unless mig-version
	     (return-from migrate :skipped))
	   (unless version
	     (setf version mig-version))
	   (cond ((> version mig-version) 
		  (warn "Migration not present.")
		  (return-from migrate :skipped))
		 ((equal db-version version) (warn "Nothing to migrate."))
		 (t (format t "~%~%-----------------~%Total migrations: ~S" 
			    (exec-migrations db-version version)))))
      (disconnect))))
