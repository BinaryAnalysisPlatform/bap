;;; Memcheck - memory access monitor kit
;;;
;;;
;;;
;;; Incidents:
;;; - (incident double-release acquire release release)
;;;   reported when the same memory region is released twice;
;;;   reported when a region that was never acquired is released;
;;; - (incident use-after-release acquire release use)
;;;   reported when a memory access operation occurs on a memory
;;;   region that was released
;;;
;;; Attributes:
;;; - memcheck/site/acquire
;;; - memcheck/site/release
;;; - memcheck/site/double-release
;;; - memcheck/site/use-after-release
;;;
;;;
;;; State:
;;; - memcheck/dead-heap
;;; - memcheck/live-heap

(require incident)

;; Public Interface

(defun memcheck-is-tracked (heap ptr)
  (or
   (region-contains (symbol-concat 'memcheck/live heap) ptr)
   (region-contains (symbol-concat 'memcheck/dead heap) ptr)))

(defun memcheck-release (heap ptr)
  (let ((dead (region-contains (symbol-concat 'memcheck/dead heap) ptr)))
    (if dead (memcheck/report-double-release ptr)
      (let ((live (region-contains (symbol-concat 'memcheck/live heap) ptr)))
        (when (= live ptr)
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
      (when (/= b1 b2)
        (memcheck/report-out-of-bound b1 b2))
      (when (/= r1 r2)
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
  (memcheck/register ptr 'memcheck/site/double-release)
  (report-incident 'memcheck-double-release ptr
                   'memcheck/site/acquire
                   'memcheck/site/release
                   'memcheck/site/double-release))


(defun memcheck/report-out-of-bound (r1 r2)
  (let ((r1 (dict-get 'memcheck/site/acquire r1))
        (r2 (dict-get 'memcheck/site/acquire r2)))
    (incident-report 'memcheck-out-of-bound
                   (or r1 r2)
                   (or r2 r1)
                   (incident-location))))

(defun memcheck/register (ptr site)
  (dict-add site ptr (incident-location)))
