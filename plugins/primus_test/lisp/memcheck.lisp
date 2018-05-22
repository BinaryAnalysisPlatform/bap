;;; Memcheck - memory access monitor kit
;;;
;;;
;;;
;;; Incidents:
;;; - (incident double-release acquire release release)
;;;   reported when the same memory region is released twice;
;;; - (incident corrupted-release release)
;;;   reported when a region that was never acquired is released;
;;; - (incident use-after-release acquire release use)
;;;   reported when a memory access opeartion occurs on a memory
;;;   region that was released
;;;
;;; Attributes:
;;; - memcheck/site/acquire
;;; - memcheck/site/release
;;; - memcheck/site/double-release
;;; - memcheck/site/corrupted-release
;;; - memcheck/site/use-after-release
;;;
;;;
;;; State:
;;; - memcheck/dead-heap
;;; - memcheck/live-heap

(require incident)

;; Public Interface
(defun memcheck-release (heap ptr)
  (let ((dead (region-contains (symbol-concat 'memcheck/dead heap) ptr)))
    (if dead (memcheck/report-double-release ptr)
      (let ((live (region-contains (symbol-concat 'memcheck/live heap) ptr)))
        (if (or (not live) (not (= live ptr)))
            (memcheck/report-corrupted-release ptr)
          (memcheck/register ptr 'memcheck/site/release)
          (region-move (symbol-concat 'memcheck/dead heap)
                       (symbol-concat 'memcheck/live heap) ptr))))))

(defun memcheck-acquire (heap ptr len)
  (region-create (symbol-concat 'memcheck/live heap) ptr (-1 (+ ptr len)))
  (memcheck/register ptr 'memcheck/site/acquire))

(defun memcheck-access (heap ptr)
  (let ((reg (region-contains (symbol-concat 'memcheck/dead heap) ptr)))
    (when reg (memcheck/report-use-after-release reg))))

(defun memcheck-bounds (heap beg len)
  (when (and beg len)
    (let ((end (-1 (+ beg len)))
          (r1 (region-contains (symbol-concat 'memcheck/live heap) beg))
          (r2 (region-contains (symbol-concat 'memcheck/live heap) end)))
      (when (not (= r1 r2))
        (memcheck/report-out-of-bound r1 r2)))))

;; Private
;; reports
(defun memcheck/report-use-after-release (ptr)
  (memcheck/register ptr 'memcheck/site/use-after-release)
  (report-incident 'memcheck-use-after-release ptr
                   'memcheck/site/acquire
                   'memcheck/site/release
                   'memcheck/site/use-after-release))

(defun memcheck/report-double-release (ptr)
  (memcheck/register ptr 'memcheck/site/double-release ptr)
  (report-incident 'memcheck-double-release ptr
                   'memcheck/site/acquire
                   'memcheck/site/release
                   'memcheck/site/double-release))

(defun memcheck/report-corrupted-release (ptr)
  (memcheck/register ptr 'memcheck/site/corrupted-release)
  (report-incident 'memcheck-corrupted-release ptr
                   'memcheck/site/corrupted-release))

(defun memcheck/report-out-of-bound (r1 r2)
  (incident-report 'memcheck-out-of-bound
                   (dict-get 'memcheck/site/acquire r1)
                   (dict-get 'memcheck/site/acquire r2)
                   (incident-location)))

(defun memcheck/register (ptr site)
  (dict-add site ptr (incident-location)))
