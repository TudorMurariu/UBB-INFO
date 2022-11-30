;;; handle the posix functions
;;; Sam Steingold 1999

(in-package "POSIX")

(export
 '(resolve-host-ipaddr bogomips
   stream-lock duplicate-handle copy-file
   hostent hostent-name hostent-aliases hostent-addr-list hostent-addrtype
   erf erfc j0 j1 jn y0 y1 yn gamma lgamma))

;;; ============================================================
(defstruct hostent
  "see gethostbyname(3) for details"
  (name "" :type simple-string)
  (aliases nil :type list)
  (addr-list nil :type list)
  (addrtype 2 :type fixnum))

(defun resolve-host-ipaddr (&optional (host :default))
  (if host
      (multiple-value-bind (name aliases addr-list addrtype)
          (resolve-host-ipaddr-internal host)
        (make-hostent :name name :aliases aliases
                      :addr-list addr-list :addrtype addrtype))
      (let ((li (resolve-host-ipaddr-internal nil)))
        (map-into li (lambda (he)
                       (make-hostent
                        :name (svref he 0) :aliases (svref he 1)
                        :addr-list (svref he 2) :addrtype (svref he 3)))
                  li))))

;;; ============================================================
#+unix (progn
(export
 '(user-data user-data-login-id user-data-passwd user-data-uid user-data-gid
   user-data-full-name user-data-shell))

(defstruct user-data
  "see stat(2) for details"
  (login-id  "" :type simple-string)
  (passwd    "" :type simple-string)
  (uid        0 :type (unsigned-byte 32))
  (gid        0 :type (unsigned-byte 32))
  (full-name "" :type simple-string)
  (home-dir  "" :type simple-string)
  (shell     "" :type simple-string))

(defmethod print-object ((ud user-data) (out stream))
  (if *print-readably* (call-next-method)
      (format out "~a:~a:~d:~d:~a:~a:~a"
              (user-data-login-id ud) (user-data-passwd ud)
              (user-data-uid ud) (user-data-gid ud) (user-data-full-name ud)
              (user-data-home-dir ud) (user-data-shell ud))))

(defun user-data (&optional (user :default))
  (if user
      (multiple-value-bind (login-id passwd uid gid full-name home-dir shell)
          (user-data-internal user)
        (make-user-data :login-id login-id :passwd passwd :uid uid :gid gid
                        :full-name full-name :home-dir home-dir :shell shell))
      (let ((li (user-data-internal nil)))
        (map-into li (lambda (ud)
                       (make-user-data
                        :login-id (svref ud 0) :passwd (svref ud 1)
                        :uid (svref ud 2) :gid (svref ud 3)
                        :full-name (svref ud 4) :home-dir (svref ud 5)
                        :shell (svref ud 6)))
                  li))))
)
;;; ============================================================
#+unix (progn
(export
 '(file-stat file-stat-file file-stat-dev file-stat-ino file-stat-mode
   file-stat-nlink file-stat-uid file-stat-gid file-stat-rdev
   file-stat-size file-stat-blksize file-stat-blocks file-stat-atime
   file-stat-mtime file-stat-ctime))

(defstruct file-stat
  file
  (dev     0 :type (unsigned-byte 32))
  (ino     0 :type (unsigned-byte 32))
  (mode    0 :type (unsigned-byte 32))
  (nlink   0 :type (unsigned-byte 32))
  (uid     0 :type (unsigned-byte 32))
  (gid     0 :type (unsigned-byte 32))
  (rdev    0 :type (unsigned-byte 32))
  (size    0 :type (unsigned-byte 32))
  (blksize 0 :type (unsigned-byte 32))
  (blocks  0 :type (unsigned-byte 32))
  (atime   0 :type (integer 0))
  (mtime   0 :type (integer 0))
  (ctime   0 :type (integer 0)))

(defun file-stat (file &optional link-p)
  "return an instance of the FILE-STAT structure for the object specified"
  (multiple-value-bind (file dev ino mode nlink uid gid rdev size
                        blksize blocks atime mtime ctime)
      (file-stat-internal file link-p)
    (make-file-stat :file file :dev dev :ino ino :mode mode :nlink nlink
                    :uid uid :gid gid :rdev rdev :size size :blksize blksize
                    :blocks blocks :atime atime :mtime mtime :ctime ctime)))
)

;;; ============================================================
#+unix (progn
(export
 '(sysinfo sysinfo-sysname sysinfo-nodename
   sysinfo-release sysinfo-version sysinfo-machine sysinfo-page-size
   sysinfo-physical-pages sysinfo-physical-pages-available
   sysinfo-num-processor-conf sysinfo-num-processor-online
   sysinfo-max-threads-per-process))


(defstruct sysinfo
  "see uname(2) and sysconf(3c) for details"
  ;; from uname
  (sysname      "" :type simple-string)
  (nodename     "" :type simple-string)
  (release      "" :type simple-string)
  (version      "" :type simple-string)
  (machine      "" :type simple-string)
  ;; from sysconf
  (page-size       nil :type (or null (eq t) (unsigned-byte 32)))
  (physical-pages  nil :type (or null (eq t) (unsigned-byte 32)))
  (physical-pages-available nil :type (or null (eq t) (unsigned-byte 32)))
  (num-processor-conf   nil :type (or null (eq t) (unsigned-byte 32)))
  (num-processor-online nil :type (or null (eq t) (unsigned-byte 32)))
  (max-threads-per-process nil :type (or null (eq t) (unsigned-byte 32))))

(defun sysinfo ()
  "Return an instance of the SYSINFO structure.
NIL - no such key; T - sysconf(3c) returned -1."
  (multiple-value-bind
        (sysname nodename release version machine
         page-size physical-pages physical-pages-available
         num-processor-conf num-processor-online max-threads-per-process)
      (sysinfo-internal)
    (make-sysinfo :sysname sysname :nodename nodename :release release
                  :version version :machine machine
                  :page-size page-size :physical-pages physical-pages
                  :physical-pages-available physical-pages-available
                  :num-processor-conf num-processor-conf
                  :num-processor-online num-processor-online
                  :max-threads-per-process max-threads-per-process)))
)
;;; ============================================================
#+unix (progn
(export
 '(resource-usage-limits rlimit rlimit-soft rlimit-hard
   limits limits-core limits-cpu limits-heap limits-file-size limits-num-files
   limits-stack limits-virt-mem limits-rss limits-memlock
   usage usage-user-fime usage-system-time usage-max-rss usage-int-rss
   usage-minor-page-faults usage-major-page-faults usage-num-swaps
   usage-blocks-input usage-blocks-output usage-messages-sent
   usage-messages-received usage-signals usage-context-switches-voluntary
   usage-context-switches-involuntary))

(defstruct rlimit
  "see getrlimit(2) for details"
  (soft nil :type (or null (unsigned-byte 32)))
  (hard nil :type (or null (unsigned-byte 32))))

(defmethod print-object ((rl rlimit) (out stream))
  (if *print-readably* (call-next-method)
      (format out "~a:~a" (rlimit-soft rl) (rlimit-hard rl))))

(defstruct limits
  "see getrlimit(2) for details"
  (core nil :type (or null rlimit))
  (cpu  nil :type (or null rlimit))
  (heap nil :type (or null rlimit))
  (file-size nil :type (or null rlimit))
  (num-files nil :type (or null rlimit))
  (stack nil :type (or null rlimit))
  (virt-mem nil :type (or null rlimit))
  (rss nil :type (or null rlimit))
  (memlock nil :type (or null rlimit)))

(defstruct usage
  "see getrusage(3) for details"
  (user-time 0.0d0 :type double-float)
  (system-time 0.0d0 :type double-float)
  (max-rss 0 :type (signed-byte 32))
  (int-rss 0 :type (signed-byte 32))
  (minor-page-faults 0 :type (signed-byte 32))
  (major-page-faults 0 :type (signed-byte 32))
  (num-swaps 0 :type (signed-byte 32))
  (blocks-input 0 :type (signed-byte 32))
  (blocks-output 0 :type (signed-byte 32))
  (messages-sent 0 :type (signed-byte 32))
  (messages-received 0 :type (signed-byte 32))
  (signals 0 :type (signed-byte 32))
  (context-switches-voluntary 0 :type (signed-byte 32))
  (context-switches-involuntary 0 :type (signed-byte 32)))

(defun resource-usage-limits ()
  "return 3 values - 2 USAGE structures, for this process
and for the children, and a LIMITS structure.
see getrusage(3) and getrlimit(2) for details"
  (multiple-value-bind
        (u11 u11a u12 u12a u13 u14 u15 u16 u17 u18 u19 u110 u111 u112 u113 u114
         u21 u21a u22 u22a u23 u24 u25 u26 u27 u28 u29 u210 u211 u212 u213 u214
         lim11 lim12 lim21 lim22 lim31 lim32 lim41 lim42 lim51 lim52
         lim61 lim62 lim71 lim72 lim81 lim82 lim91 lim92)
      (resource-usage-limits-internal)
    (values
     (make-usage :user-time (+ 0.0d0 u11 (* 0.000001d0 u11a))
                 :system-time (+ 0.0d0 u12 (* 0.000001d0 u12a))
                 :max-rss u13 :int-rss u14
                 :minor-page-faults u15 :major-page-faults u16
                 :num-swaps u17
                 :blocks-input u18 :blocks-output u19
                 :messages-sent u110 :messages-received u111
                 :signals u112 :context-switches-voluntary u113
                 :context-switches-involuntary u114)
     (make-usage :user-time (+ 0.0d0 u21 (* 0.000001d0 u21a))
                 :system-time (+ 0.0d0 u22 (* 0.000001d0 u22a))
                 :max-rss u23 :int-rss u24
                 :minor-page-faults u25 :major-page-faults u26
                 :num-swaps u27
                 :blocks-input u28 :blocks-output u29
                 :messages-sent u210 :messages-received u211
                 :signals u212 :context-switches-voluntary u213
                 :context-switches-involuntary u214)
     (labels ((nu (lim) (if (eq lim t) nil lim))
              (mk (l1 l2) (if l1 (make-rlimit :soft (nu l1) :hard (nu l2)))))
       (make-limits :core (mk lim11 lim12) :cpu (mk lim21 lim22)
                    :heap (mk lim31 lim32) :file-size (mk lim41 lim42)
                    :num-files (mk lim51 lim52) :stack (mk lim61 lim62)
                    :virt-mem (mk lim71 lim72) :rss (mk lim81 lim82)
                    :memlock (mk lim91 lim92))))))
)

(use-package '("POSIX") "EXT")
(ext:re-export "POSIX" "EXT")
