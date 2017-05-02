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

(defvar *config-pathname* (make-pathname :name ".migrate" 
					 :type "conf" 
					 :defaults (user-homedir-pathname)))
(defvar *migration-dir* nil)

(defvar +all-db-types+
  '(:postgresql :postgresql-socket :mysql :sqlite :sqlite3 :odbc :oracle
    #+allegro :aodbc))

;; You can change your schema table name here.
(defvar *schema-table-name* "schema_info")

(defclass conn-specs ()
  ((aodbc :accessor aodbc-spec :initform nil)
   (mysql :accessor mysql-spec :initform nil)
   (postgresql :accessor postgresql-spec :initform nil)
   (postgresql-socket :accessor postgresql-socket-spec :initform nil)
   (sqlite :accessor sqlite-spec :initform nil)
   (sqlite3 :accessor sqlite3-spec :initform nil)
   (odbc :accessor odbc-spec :initform nil)
   (oracle :accessor oracle-spec :initform nil))
  (:documentation "Connection specs for CLSQL for different databases"))

(defun resolve-migration-dir (specs)
  (let ((migration-conf (second (second specs))))
    (typecase (first migration-conf)
      (string
       (first migration-conf))
      (keyword
       (when (eql (first migration-conf) :relative)
         (namestring
          (merge-pathnames (second migration-conf)
                           (make-pathname :name nil
                                          :type nil
                                          :defaults *config-pathname*))))))))

(defun read-specs (&optional (path *config-pathname*))
  "Read database and migrations directory specs and set respective variables."
  (if (probe-file path)
      (with-open-file (stream path :direction :input)
	(let ((spec (make-instance 'conn-specs))
	      (specs (read stream)))
	  (setf (slot-value spec 
			    (intern (symbol-name (first (first specs)))
				    (find-package '#:cl-migrations)))
		(second (first specs)))
	  (setf *migration-dir* (resolve-migration-dir specs))
	  (unless (or (null *migration-dir*)
                      (equal (subseq *migration-dir* (1- (length *migration-dir*))) "/"))
	    ;;Looks like the user forgot add a trailing slash - fix this.
	    (setf *migration-dir* (concatenate 'string *migration-dir* "/")))
	  (format t "~%Setting up migrations directory: ~S" *migration-dir*)
	  spec))))

(defun spec-fn (db-type)
  (intern (concatenate 'string (symbol-name db-type)
		       (symbol-name '#:-spec))
	  (find-package '#:cl-migrations)))

(defun db-type-spec (db-type spec)
  (funcall (spec-fn db-type) spec))

(defun init-connection (db-type spec &optional clean)
  "Connect to the database; Truncate database if clean flag is set."
  (clsql:connect spec
		 :database-type db-type
		 :make-default t
		 :if-exists :old)
  ;; Ensure database is empty
  (when clean
    (warn "Clean flag set, truncating database ~S" (second spec))
    (truncate-database :database *default-database*))
  *default-database*)

(defun load-necessary-systems (spec &optional clean)
  "Load the matching database type"
  (dolist (db-type +all-db-types+)
    (when (db-type-spec db-type spec)
      (clsql-sys:initialize-database-type :database-type db-type)
      (init-connection db-type (db-type-spec db-type spec) clean)
      (return))))

(defun create-schema-table ()
  "Check for schema table and create if it does not exist."
  (if (table-exists-p *schema-table-name*)
      (format t "~%Schema table exists.")
      (progn 
	(format t "~%Schema table doesn't exist, creating.")
	(create-table *schema-table-name* '((version (varchar 500))))
	;;Initialize the version number to 0.
	(insert-records :into *schema-table-name*
			:attributes '(version)
			:values '(0)))))

(defun init-config (&optional clean)
  "Load database configuration and create a connection if none exists."
  (let ((spec (read-specs))
	(error-count 0))
    (unless spec
      (warn "Exiting - .migrate.conf not found in the home directory")
      (incf error-count)
      (return-from init-config :skipped))
    (load-necessary-systems spec clean)
    (unless *default-database*
      (warn "Unable to connect to database. Please check your CLSQL setup.")
      (incf error-count)
      (return-from init-config :skipped))
    ;;The spec works, so we create the schema table
    (create-schema-table)
    (format t "~%Database ready.")
    (zerop error-count)))

(defun connect-db ()
  (if (init-config)
      (format t "~%Be sure run disconnect-db when you are done.")))

(defun disconnect-db ()
  (if *default-database*
      (disconnect)))
