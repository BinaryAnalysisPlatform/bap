(declare (context (target armv8-a+le)))

(in-package aarch64)

;;; LDs..
;;; NOTE:
;;;   encodings do not CheckFPAdvSIMDEnabled64(), HaveMTE2Ext(), 
;;;   SetTagCheckedInstruction(), CheckSPAlignment()

;; LD1 (multiple structures, post index, four registers)

(defmacro LD1Fourv._POST (elems bytes va base off)
  (LD..v._POST 4 elems 1 bytes va base off))

(defun LD1Fourv8b_POST (_ da_db_dc_dd xn xm) (LD1Fourv._POST 8 1 da_db_dc_dd xn xm))
(defun LD1Fourv16b_POST (_ qa_qb_qc_qd xn xm) (LD1Fourv._POST 16 1 qa_qb_qc_qd xn xm))
(defun LD1Fourv4h_POST (_ da_db_dc_dd xn xm) (LD1Fourv._POST 4 2 da_db_dc_dd xn xm))
(defun LD1Fourv8h_POST (_ qa_qb_qc_qd xn xm) (LD1Fourv._POST 8 2 qa_qb_qc_qd xn xm))
(defun LD1Fourv2s_POST (_ da_db_dc_dd xn xm) (LD1Fourv._POST 2 4 da_db_dc_dd xn xm))
(defun LD1Fourv4s_POST (_ qa_qb_qc_qd xn xm) (LD1Fourv._POST 4 4 qa_qb_qc_qd xn xm))
(defun LD1Fourv1d_POST (_ da_db_dc_dd xn xm) (LD1Fourv._POST 1 8 da_db_dc_dd xn xm))
(defun LD1Fourv2d_POST (_ qa_qb_qc_qd xn xm) (LD1Fourv._POST 2 8 qa_qb_qc_qd xn xm))

;; LD1 (multiple structures, post index, three registers)

(defmacro LD1Threev._POST (elems bytes va base off)
  (LD..v._POST 3 elems 1 bytes va base off))

(defun LD1Threev8b_POST (_ da_db_dc xn xm) (LD1Threev._POST 8 1 da_db_dc xn xm))
(defun LD1Threev16b_POST (_ qa_qb_qc xn xm) (LD1Threev._POST 16 1 qa_qb_qc xn xm))
(defun LD1Threev4h_POST (_ da_db_dc xn xm) (LD1Threev._POST 4 2 da_db_dc xn xm))
(defun LD1Threev8h_POST (_ qa_qb_qc xn xm) (LD1Threev._POST 8 2 qa_qb_qc xn xm))
(defun LD1Threev2s_POST (_ da_db_dc xn xm) (LD1Threev._POST 2 4 da_db_dc xn xm))
(defun LD1Threev4s_POST (_ qa_qb_qc xn xm) (LD1Threev._POST 4 4 qa_qb_qc xn xm))
(defun LD1Threev1d_POST (_ da_db_dc xn xm) (LD1Threev._POST 1 8 da_db_dc xn xm))
(defun LD1Threev2d_POST (_ qa_qb_qc xn xm) (LD1Threev._POST 2 8 qa_qb_qc xn xm))

;; LD1 (multiple structures, post index, two registers)

(defmacro LD1Twov._POST (elems bytes va base off)
  (LD..v._POST 2 elems 1 bytes va base off))

(defun LD1Twov8b_POST (_ da_db xn xm) (LD1Twov._POST 8 1 da_db xn xm))
(defun LD1Twov16b_POST (_ qa_qb xn xm) (LD1Twov._POST 16 1 qa_qb xn xm))
(defun LD1Twov4h_POST (_ da_db xn xm) (LD1Twov._POST 4 2 da_db xn xm))
(defun LD1Twov8h_POST (_ qa_qb xn xm) (LD1Twov._POST 8 2 qa_qb xn xm))
(defun LD1Twov2s_POST (_ da_db xn xm) (LD1Twov._POST 2 4 da_db xn xm))
(defun LD1Twov4s_POST (_ qa_qb xn xm) (LD1Twov._POST 4 4 qa_qb xn xm))
(defun LD1Twov1d_POST (_ da_db xn xm) (LD1Twov._POST 1 8 da_db xn xm))
(defun LD1Twov2d_POST (_ qa_qb xn xm) (LD1Twov._POST 2 8 qa_qb xn xm))

;; LD1 (multiple structures, post index, one register)

(defmacro LD1Onev._POST (elems bytes va base off)
  (LD..v._POST 1 elems 1 bytes va base off))

(defun LD1Onev8b_POST (_ da xn xm) (LD1Onev._POST 8 1 da xn xm))
(defun LD1Onev16b_POST (_ qa xn xm) (LD1Onev._POST 16 1 qa xn xm))
(defun LD1Onev4h_POST (_ da xn xm) (LD1Onev._POST 4 2 da xn xm))
(defun LD1Onev8h_POST (_ qa xn xm) (LD1Onev._POST 8 2 qa xn xm))
(defun LD1Onev2s_POST (_ da xn xm) (LD1Onev._POST 2 4 da xn xm))
(defun LD1Onev4s_POST (_ qa xn xm) (LD1Onev._POST 4 4 qa xn xm))
(defun LD1Onev1d_POST (_ da xn xm) (LD1Onev._POST 1 8 da xn xm))
(defun LD1Onev2d_POST (_ qa xn xm) (LD1Onev._POST 2 8 qa xn xm))

;; LD1 (multiple structures, no offset, four registers)

(defmacro LD1Fourv. (elems bytes va base)
  (LD 4 elems 1 base bytes va))

(defun LD1Fourv8b (da_db_dc_dd xn) (LD1Fourv. 8 1 da_db_dc_dd xn))
(defun LD1Fourv16b (qa_qb_qc_qd xn) (LD1Fourv. 16 1 qa_qb_qc_qd xn))
(defun LD1Fourv4h (da_db_dc_dd xn) (LD1Fourv. 4 2 da_db_dc_dd xn))
(defun LD1Fourv8h (qa_qb_qc_qd xn) (LD1Fourv. 8 2 qa_qb_qc_qd xn))
(defun LD1Fourv2s (da_db_dc_dd xn) (LD1Fourv. 2 4 da_db_dc_dd xn))
(defun LD1Fourv4s (qa_qb_qc_qd xn) (LD1Fourv. 4 4 qa_qb_qc_qd xn))
(defun LD1Fourv1d (da_db_dc_dd xn) (LD1Fourv. 1 8 da_db_dc_dd xn))
(defun LD1Fourv2d (qa_qb_qc_qd xn) (LD1Fourv. 2 8 qa_qb_qc_qd xn))

;; LD1 (multiple structures, no offset, three registers)

(defmacro LD1Threev. (elems bytes va base)
  (LD 3 elems 1 base bytes va))

(defun LD1Threev8b (da_db_dc xn) (LD1Threev. 8 1 da_db_dc xn))
(defun LD1Threev16b (qa_qb_qc xn) (LD1Threev. 16 1 qa_qb_qc xn))
(defun LD1Threev4h (da_db_dc xn) (LD1Threev. 4 2 da_db_dc xn))
(defun LD1Threev8h (qa_qb_qc xn) (LD1Threev. 8 2 qa_qb_qc xn))
(defun LD1Threev2s (da_db_dc xn) (LD1Threev. 2 4 da_db_dc xn))
(defun LD1Threev4s (qa_qb_qc xn) (LD1Threev. 4 4 qa_qb_qc xn))
(defun LD1Threev1d (da_db_dc xn) (LD1Threev. 1 8 da_db_dc xn))
(defun LD1Threev2d (qa_qb_qc xn) (LD1Threev. 2 8 qa_qb_qc xn))

;; LD1 (multiple structures, no offset, two registers)

(defmacro LD1Twov. (elems bytes va base)
  (LD 2 elems 1 base bytes va))

(defun LD1Twov8b (da_db xn) (LD1Twov. 8 1 da_db xn))
(defun LD1Twov16b (qa_qb xn) (LD1Twov. 16 1 qa_qb xn))
(defun LD1Twov4h (da_db xn) (LD1Twov. 4 2 da_db xn))
(defun LD1Twov8h (qa_qb xn) (LD1Twov. 8 2 qa_qb xn))
(defun LD1Twov2s (da_db xn) (LD1Twov. 2 4 da_db xn))
(defun LD1Twov4s (qa_qb xn) (LD1Twov. 4 4 qa_qb xn))
(defun LD1Twov1d (da_db xn) (LD1Twov. 1 8 da_db xn))
(defun LD1Twov2d (qa_qb xn) (LD1Twov. 2 8 qa_qb xn))

;; LD1 (multiple structures, no offset, one register)

(defmacro LD1Onev. (elems bytes va base)
  (LD 1 elems 1 base bytes va))

(defun LD1Onev8b (da xn) (LD1Onev. 8 1 da xn))
(defun LD1Onev16b (qa xn) (LD1Onev. 16 1 qa xn))
(defun LD1Onev4h (da xn) (LD1Onev. 4 2 da xn))
(defun LD1Onev8h (qa xn) (LD1Onev. 8 2 qa xn))
(defun LD1Onev2s (da xn) (LD1Onev. 2 4 da xn))
(defun LD1Onev4s (qa xn) (LD1Onev. 4 4 qa xn))
(defun LD1Onev1d (da xn) (LD1Onev. 1 8 da xn))
(defun LD1Onev2d (qa xn) (LD1Onev. 2 8 qa xn))

;; LD2 (multiple structures, post index)

(defun LD2Twov8b_POST (_ da_db xn xm) (LD2Twov._POST da_db xn xm 8 1))
(defun LD2Twov16b_POST (_ qa_qb xn xm) (LD2Twov._POST qa_qb xn xm 16 1))
(defun LD2Twov4h_POST (_ da_db xn xm) (LD2Twov._POST da_db xn xm 4 2))
(defun LD2Twov8h_POST (_ qa_qb xn xm) (LD2Twov._POST qa_qb xn xm 8 2))
(defun LD2Twov2s_POST (_ da_db xn xm) (LD2Twov._POST da_db xn xm 2 4))
(defun LD2Twov4s_POST (_ qa_qb xn xm) (LD2Twov._POST qa_qb xn xm 4 4))
(defun LD2Twov2d_POST (_ qa_qb xn xm) (LD2Twov._POST qa_qb xn xm 2 8))

(defmacro LD2Twov._POST (va_vb xn xm elems bytes)
  "(LD2Twov._POST va_vb xn elesms bytes) loads multiple 2-element structures from
   memory at address xn with offset xm and stores it in va and vb with de-interleaving."
  (LD..v._POST 1 elems 2 bytes va_vb xn xm))

;; LD2 (multiple structures, no offset)

(defun LD2Twov8b (da_db xn) (LD2Twov. da_db xn 8 1))
(defun LD2Twov16b (qa_qb xn) (LD2Twov. qa_qb xn 16 1))
(defun LD2Twov4h (da_db xn) (LD2Twov. da_db xn 4 2))
(defun LD2Twov8h (qa_qb xn) (LD2Twov. qa_qb xn 8 2))
(defun LD2Twov2s (da_db xn) (LD2Twov. da_db xn 2 4))
(defun LD2Twov4s (qa_qb xn) (LD2Twov. qa_qb xn 4 4))
(defun LD2Twov2d (qa_qb xn) (LD2Twov. qa_qb xn 2 8))

(defmacro LD2Twov. (va_vb xn elems bytes)
  "(LD2Twov. va_vb xn elesms bytes) loads multiple 2-element structures from
   memory at address xn and stores it in va and vb with de-interleaving."
    (LD 1 elems 2 xn bytes va_vb))

;; LD3 (multiple structures, post index)

(defun LD3Threev8b_POST (_ da_db_dc xn xm) (LD3Threev._POST da_db_dc xn xm 8 1))
(defun LD3Threev16b_POST (_ qa_qb_qc xn xm) (LD3Threev._POST qa_qb_qc xn xm 16 1))
(defun LD3Threev4h_POST (_ da_db_dc xn xm) (LD3Threev._POST da_db_dc xn xm 4 2))
(defun LD3Threev8h_POST (_ qa_qb_qc xn xm) (LD3Threev._POST qa_qb_qc xn xm 8 2))
(defun LD3Threev2s_POST (_ da_db_dc xn xm) (LD3Threev._POST da_db_dc xn xm 2 4))
(defun LD3Threev4s_POST (_ qa_qb_qc xn xm) (LD3Threev._POST qa_qb_qc xn xm 4 4))
(defun LD3Threev2d_POST (_ qa_qb_qc xn xm) (LD3Threev._POST qa_qb_qc xn xm 2 8))

(defmacro LD3Threev._POST (va_vb_vc xn xm elems bytes)
  "(LD3Threev._POST va_vb_vc xn xm elems bytes) loads multiple 3-element structures
   from memory at address xn with offset xm and stores it in va, vb and vc with de-interleaving."
  (LD..v._POST 1 elems 3 bytes va_vb_vc xn xm))

;; LD3 (multiple structures, no offset)

(defun LD3Threev8b (da_db_dc xn) (LD3Threev. da_db_dc xn 8 1))
(defun LD3Threev16b (qa_qb_qc xn) (LD3Threev. qa_qb_qc xn 16 1))
(defun LD3Threev4h (da_db_dc xn) (LD3Threev. da_db_dc xn 4 2))
(defun LD3Threev8h (qa_qb_qc xn) (LD3Threev. qa_qb_qc xn 8 2))
(defun LD3Threev2s (da_db_dc xn) (LD3Threev. da_db_dc xn 2 4))
(defun LD3Threev4s (qa_qb_qc xn) (LD3Threev. qa_qb_qc xn 4 4))
(defun LD3Threev2d (qa_qb_qc xn) (LD3Threev. qa_qb_qc xn 2 8))

(defmacro LD3Threev. (va_vb_vc xn elems bytes)
  "(LD3Threev. va_vb_vc xn elems bytes) loads multiple 3-element structures from
   memory at address xn and stores it in va, vb and vc with de-interleaving."
    (LD 1 elems 3 xn bytes va_vb_vc))

;; LD4 (multiple structures, post index)

(defun LD4Fourv8b_POST (_ da_db_dc_dd xn xm) (LD4Fourv._POST da_db_dc_dd xn xm 8 1))
(defun LD4Fourv16b_POST (_ qa_qb_qc_qd xn xm) (LD4Fourv._POST qa_qb_qc_qd xn xm 16 1))
(defun LD4Fourv4h_POST (_ da_db_dc_dd xn xm) (LD4Fourv._POST da_db_dc_dd xn xm 4 2))
(defun LD4Fourv8h_POST (_ qa_qb_qc_qd xn xm) (LD4Fourv._POST qa_qb_qc_qd xn xm 8 2))
(defun LD4Fourv2s_POST (_ da_db_dc_dd xn xm) (LD4Fourv._POST da_db_dc_dd xn xm 2 4))
(defun LD4Fourv4s_POST (_ qa_qb_qc_qd xn xm) (LD4Fourv._POST qa_qb_qc_qd xn xm 4 4))
(defun LD4Fourv2d_POST (_ qa_qb_qc_qd xn xm) (LD4Fourv._POST qa_qb_qc_qd xn xm 2 8))

(defmacro LD4Fourv._POST (va_vb_vc xn xm elems bytes)
  "(LD4Fourv._POST va_vb_vc xn xm elems bytes) loads multiple 4-element structures
   from memory at address xn with offset xm and stores it in va, vb, vc and vd with de-interleaving."
  (LD..v._POST 1 elems 4 bytes va_vb_vc xn xm))

;; LD4 (multiple structures, no offset)

(defun LD4Fourv8b (da_db_dc_dd xn) (LD4Fourv. da_db_dc_dd xn 8 1))
(defun LD4Fourv16b (qa_qb_qc_qd xn) (LD4Fourv. qa_qb_qc_qd xn 16 1))
(defun LD4Fourv4h (da_db_dc_dd xn) (LD4Fourv. da_db_dc_dd xn 4 2))
(defun LD4Fourv8h (qa_qb_qc_qd xn) (LD4Fourv. qa_qb_qc_qd xn 8 2))
(defun LD4Fourv2s (da_db_dc_dd xn) (LD4Fourv. da_db_dc_dd xn 2 4))
(defun LD4Fourv4s (qa_qb_qc_qd xn) (LD4Fourv. qa_qb_qc_qd xn 4 4))
(defun LD4Fourv2d (qa_qb_qc_qd xn) (LD4Fourv. qa_qb_qc_qd xn 2 8))

(defmacro LD4Fourv. (va_vb_vc xn elems bytes)
  "(LD4Fourv. va_vb_vc xn elems bytes) loads multiple 4-element structures from memory
   at address xn and stores it in va, vb, vc and vd with de-interleaving."
    (LD 1 elems 4 xn bytes va_vb_vc))

;; LD multiple struct algorithm

(defmacro LD..v._POST (rpt elems selems bytes grp base off)
  "(LD..v._POST rpt elems selems bytes grp base off) loads multiple selems-element structs
   from memory address base with offset off and stores them in the vector list grp."
  (prog
    (LD rpt elems selems base bytes grp)
    (if (= (symbol off) 'XZR)
      (set$ base (+ base (* rpt selems elems bytes)))
      (set$ base (+ base off)))))

(defmacro LD (rpt elems selems base bytes grp)
  "(LD rpt elems selems base bytes grp) loads multiple selems-element structs from memory address base."
  (insert-with-de-interleaving 0 rpt elems selems base bytes grp))

(defun insert-with-de-interleaving (r rpt elems selems base bytes grp)
  (if (> rpt 1)
    (when (< r rpt)
      (let ((nth (nth-reg-in-group grp r)))
        (prog
          (insert-a 1 0 elems base bytes (symbol nth) 0 0 0 0)
          (insert-with-de-interleaving 
            (+ r 1) rpt elems selems (+ base (* elems bytes)) bytes grp))))
    (insert-a selems 0 elems base bytes grp 0 0 0 0)))

(defun insert-a (selems e elems addr bytes grp acc-a acc-b acc-c acc-d)
  (if (< e elems)
    (let ((acc-a (if (= e 0) (mem-read addr bytes) (concat acc-a (mem-read addr bytes)))))
      (if (> selems 1)
        (insert-b selems e elems (+ addr bytes) bytes grp acc-a acc-b acc-c acc-d)
        (insert-a selems (+ e 1) elems (+ addr bytes) bytes grp acc-a acc-b acc-c acc-d)))
    (prog
      (when (<= 1 selems) (set$ (nth-reg-in-group grp 0) acc-a))
      (when (<= 2 selems) (set$ (nth-reg-in-group grp 1) acc-b))
      (when (<= 3 selems) (set$ (nth-reg-in-group grp 2) acc-c))
      (when (<= 4 selems) (set$ (nth-reg-in-group grp 3) acc-d)))))

(defun insert-b (selems e elems addr bytes grp acc-a acc-b acc-c acc-d)
  (let ((acc-b (if (= e 0) (mem-read addr bytes) (concat acc-b (mem-read addr bytes)))))
    (if (> selems 2)
      (insert-c selems e elems (+ addr bytes) bytes grp acc-a acc-b acc-c acc-d)
      (insert-a selems (+ e 1) elems (+ addr bytes) bytes grp acc-a acc-b acc-c acc-d))))

(defun insert-c (selems e elems addr bytes grp acc-a acc-b acc-c acc-d)
  (let ((acc-c (if (= e 0) (mem-read addr bytes) (concat acc-c (mem-read addr bytes)))))
    (if (> selems 3)
      (insert-d selems e elems (+ addr bytes) bytes grp acc-a acc-b acc-c acc-d)
      (insert-a selems (+ e 1) elems (+ addr bytes) bytes grp acc-a acc-b acc-c acc-d))))

(defun insert-d (selems e elems addr bytes grp acc-a acc-b acc-c acc-d)
  (let ((acc-d (if (= e 0) (mem-read addr bytes) (concat acc-d (mem-read addr bytes)))))
    (insert-a selems (+ e 1) elems (+ addr bytes) bytes grp acc-a acc-b acc-c acc-d)))

;; LD single struct algorithm

(defmacro LD.i._POST (selems grp index base size off)
  "(LD.i._POST selems grp index base size off) loads multiple single structures from 
   address base with post index off, and inserts each structure into the index of each 
   vector register in grp."
  (prog
    (LD.i. selems grp index base size)
    (if (= (symbol off) 'XZR)
      (set$ base (+ base (/ size 8)))
      (set$ base (+ base off)))))

(defmacro LD.i. (selems grp index base size)
  "(LD.i._POST selems grp index base size off) loads multiple single structures from 
   address base, and inserts each structure into the index of each vector register in grp."
  (insert-single-element 0 selems grp index base size))

(defun insert-single-element (s selems grp index base size)
  (when (< s selems)
    (prog
      (insert-element-into-vector (nth-reg-in-group grp s)
        index (mem-read base (/ size 8)) size)
      (insert-single-element (+ s 1) selems grp index (+ base (/ size 8)) size))))

;; LD1 (single struct, no offset)

(defmacro LD1i. (qa index base size)
  (LD.i. 1 qa index base size))

(defun LD1i8 (_ qa index xn) (LD1i. qa index xn 8))
(defun LD1i16 (_ qa index xn) (LD1i. qa index xn 16))
(defun LD1i32 (_ qa index xn) (LD1i. qa index xn 32))
(defun LD1i64 (_ qa index xn) (LD1i. qa index xn 64))

;; LD1 (single struct, post index)

(defmacro LD1i._POST (qa index base size off)
  (LD.i._POST 1 qa index base size off))

(defun LD1i8_POST (_ _ qa index xn xm) (LD1i._POST qa index xn 8 xm))
(defun LD1i16_POST (_ _ qa index xn xm) (LD1i._POST qa index xn 16 xm))
(defun LD1i32_POST (_ _ qa index xn xm) (LD1i._POST qa index xn 32 xm))
(defun LD1i64_POST (_ _ qa index xn xm) (LD1i._POST qa index xn 64 xm))

;; LD2 (single struct, no offset)

(defmacro LD2i. (qa_qb index base size)
  (LD.i. 2 qa_qb index base size))

(defun LD2i8 (_ qa_qb index xn) (LD2i. qa_qb index xn 8))
(defun LD2i16 (_ qa_qb index xn) (LD2i. qa_qb index xn 16))
(defun LD2i32 (_ qa_qb index xn) (LD2i. qa_qb index xn 32))
(defun LD2i64 (_ qa_qb index xn) (LD2i. qa_qb index xn 64))

;; LD2 (single struct, post index)

(defmacro LD2i._POST (qa_qb index base size off)
  (LD.i._POST 2 qa_qb index base size off))

(defun LD2i8_POST (_ _ qa_qb index xn xm) (LD2i._POST qa_qb index xn 8 xm))
(defun LD2i16_POST (_ _ qa_qb index xn xm) (LD2i._POST qa_qb index xn 16 xm))
(defun LD2i32_POST (_ _ qa_qb index xn xm) (LD2i._POST qa_qb index xn 32 xm))
(defun LD2i64_POST (_ _ qa_qb index xn xm) (LD2i._POST qa_qb index xn 64 xm))

;; LD3 (single struct, no offset)

(defmacro LD3i. (qa_qb_qc index base size)
  (LD.i. 3 qa_qb_qc index base size))

(defun LD3i8 (_ qa_qb_qc index xn) (LD3i. qa_qb_qc index xn 8))
(defun LD3i16 (_ qa_qb_qc index xn) (LD3i. qa_qb_qc index xn 16))
(defun LD3i32 (_ qa_qb_qc index xn) (LD3i. qa_qb_qc index xn 32))
(defun LD3i64 (_ qa_qb_qc index xn) (LD3i. qa_qb_qc index xn 64))

;; LD3 (single struct, post index)

(defmacro LD3i._POST (qa_qb_qc index base size off)
  (LD.i._POST 3 qa_qb_qc index base size off))

(defun LD3i8_POST (_ _ qa_qb_qc index xn xm) (LD3i._POST qa_qb_qc index xn 8 xm))
(defun LD3i16_POST (_ _ qa_qb_qc index xn xm) (LD3i._POST qa_qb_qc index xn 16 xm))
(defun LD3i32_POST (_ _ qa_qb_qc index xn xm) (LD3i._POST qa_qb_qc index xn 32 xm))
(defun LD3i64_POST (_ _ qa_qb_qc index xn xm) (LD3i._POST qa_qb_qc index xn 64 xm))

;; LD4 (single struct, no offset)

(defmacro LD4i. (qa_qb_qc_qd index base size)
  (LD.i. 4 qa_qb_qc_qd index base size))

(defun LD4i8 (_ qa_qb_qc_qd index xn) (LD4i. qa_qb_qc_qd index xn 8))
(defun LD4i16 (_ qa_qb_qc_qd index xn) (LD4i. qa_qb_qc_qd index xn 16))
(defun LD4i32 (_ qa_qb_qc_qd index xn) (LD4i. qa_qb_qc_qd index xn 32))
(defun LD4i64 (_ qa_qb_qc_qd index xn) (LD4i. qa_qb_qc_qd index xn 64))

;; LD4 (single struct, post index)

(defmacro LD4i._POST (qa_qb_qc_qd index base size off)
  (LD.i._POST 4 qa_qb_qc_qd index base size off))

(defun LD4i8_POST (_ _ qa_qb_qc_qd index xn xm) (LD4i._POST qa_qb_qc_qd index xn 8 xm))
(defun LD4i16_POST (_ _ qa_qb_qc_qd index xn xm) (LD4i._POST qa_qb_qc_qd index xn 16 xm))
(defun LD4i32_POST (_ _ qa_qb_qc_qd index xn xm) (LD4i._POST qa_qb_qc_qd index xn 32 xm))
(defun LD4i64_POST (_ _ qa_qb_qc_qd index xn xm) (LD4i._POST qa_qb_qc_qd index xn 64 xm))

;; LD.R algorithm

(defmacro LD.Rv._POST (grp base esize dsize selems off)
  "(LD.Rv._POST grp base esize dsize selems off) loads an multiple element from a 
   base address and off post index, replicates them to the size of dsize and 
   inserts them into each vector register in group."
  (prog
    (LD.Rv. grp base esize dsize selems)
    (if (= (symbol off) 'XZR)
      (set$ base (+ base (* selems (/ dsize 8))))
      (set$ base (+ base off)))))

(defmacro LD.Rv. (grp base esize dsize selems)
  "(LD.Rv. grp base esize dsize selems) loads an multiple element from a 
   base address, replicates them to the size of dsize and 
   inserts them into each vector register in group."
  (insert-single-and-replicate grp base esize dsize selems 0))

(defun insert-single-and-replicate (grp base esize dsize selems s)
  (when (< s selems)
    (let ((element (cast-low esize (mem-read base (/ dsize esize)))))
      (prog
        (replicate-and-insert (nth-reg-in-group grp s) element esize dsize)
        (insert-single-and-replicate grp (+ base (/ dsize 8)) esize dsize selems (+ s 1))))))

;; LD1R (no offset)

(defmacro LD1Rv. (va xn esize dsize)
  (LD.Rv. (symbol va) xn esize dsize 1))

(defun LD1Rv8b (va xn) (LD1Rv. va xn 8 64))
(defun LD1Rv16b (va xn) (LD1Rv. va xn 8 128))
(defun LD1Rv4h (va xn) (LD1Rv. va xn 16 64))
(defun LD1Rv8h (va xn) (LD1Rv. va xn 16 128))
(defun LD1Rv2s (va xn) (LD1Rv. va xn 32 64))
(defun LD1Rv4s (va xn) (LD1Rv. va xn 32 128))
(defun LD1Rv1d (va xn) (LD1Rv. va xn 64 64))
(defun LD1Rv2d (va xn) (LD1Rv. va xn 64 128))

;; LD1R (post index)

(defmacro LD1Rv._POST (va xn esize dsize off)
  (LD.Rv._POST (symbol va) xn esize dsize 1 off))

(defun LD1Rv8b_POST (_ va xn xm) (LD1Rv._POST va xn 8 64 xm))
(defun LD1Rv16b_POST (_ va xn xm) (LD1Rv._POST va xn 8 128 xm))
(defun LD1Rv4h_POST (_ va xn xm) (LD1Rv._POST va xn 16 64 xm))
(defun LD1Rv8h_POST (_ va xn xm) (LD1Rv._POST va xn 16 128 xm))
(defun LD1Rv2s_POST (_ va xn xm) (LD1Rv._POST va xn 32 64 xm))
(defun LD1Rv4s_POST (_ va xn xm) (LD1Rv._POST va xn 32 128 xm))
(defun LD1Rv1d_POST (_ va xn xm) (LD1Rv._POST va xn 64 64 xm))
(defun LD1Rv2d_POST (_ va xn xm) (LD1Rv._POST va xn 64 128 xm))

;; LD2R (no offset)

(defmacro LD2Rv. (va_vb xn esize dsize)
  (LD.Rv. va_vb xn esize dsize 2))

(defun LD2Rv8b (va_vb xn) (LD2Rv. va_vb xn 8 64))
(defun LD2Rv16b (va_vb xn) (LD2Rv. va_vb xn 8 128))
(defun LD2Rv4h (va_vb xn) (LD2Rv. va_vb xn 16 64))
(defun LD2Rv8h (va_vb xn) (LD2Rv. va_vb xn 16 128))
(defun LD2Rv2s (va_vb xn) (LD2Rv. va_vb xn 32 64))
(defun LD2Rv4s (va_vb xn) (LD2Rv. va_vb xn 32 128))
(defun LD2Rv1d (va_vb xn) (LD2Rv. va_vb xn 64 64))
(defun LD2Rv2d (va_vb xn) (LD2Rv. va_vb xn 64 128))

;; LD2R (post index)

(defmacro LD2Rv._POST (va_vb xn esize dsize off)
  (LD.Rv._POST va_vb xn esize dsize 2 off))

(defun LD2Rv8b_POST (_ va_vb xn xm) (LD2Rv._POST va_vb xn 8 64 xm))
(defun LD2Rv16b_POST (_ va_vb xn xm) (LD2Rv._POST va_vb xn 8 128 xm))
(defun LD2Rv4h_POST (_ va_vb xn xm) (LD2Rv._POST va_vb xn 16 64 xm))
(defun LD2Rv8h_POST (_ va_vb xn xm) (LD2Rv._POST va_vb xn 16 128 xm))
(defun LD2Rv2s_POST (_ va_vb xn xm) (LD2Rv._POST va_vb xn 32 64 xm))
(defun LD2Rv4s_POST (_ va_vb xn xm) (LD2Rv._POST va_vb xn 32 128 xm))
(defun LD2Rv1d_POST (_ va_vb xn xm) (LD2Rv._POST va_vb xn 64 64 xm))
(defun LD2Rv2d_POST (_ va_vb xn xm) (LD2Rv._POST va_vb xn 64 128 xm))

;; LD3R (no offset)

(defmacro LD3Rv. (va_vb_vc xn esize dsize)
  (LD.Rv. va_vb_vc xn esize dsize 3))

(defun LD3Rv8b (va_vb_vc xn) (LD3Rv. va_vb_vc xn 8 64))
(defun LD3Rv16b (va_vb_vc xn) (LD3Rv. va_vb_vc xn 8 128))
(defun LD3Rv4h (va_vb_vc xn) (LD3Rv. va_vb_vc xn 16 64))
(defun LD3Rv8h (va_vb_vc xn) (LD3Rv. va_vb_vc xn 16 128))
(defun LD3Rv2s (va_vb_vc xn) (LD3Rv. va_vb_vc xn 32 64))
(defun LD3Rv4s (va_vb_vc xn) (LD3Rv. va_vb_vc xn 32 128))
(defun LD3Rv1d (va_vb_vc xn) (LD3Rv. va_vb_vc xn 64 64))
(defun LD3Rv2d (va_vb_vc xn) (LD3Rv. va_vb_vc xn 64 128))

;; LD3R (post index)

(defmacro LD3Rv._POST (va_vb_vc xn esize dsize off)
  (LD.Rv._POST va_vb_vc xn esize dsize 3 off))

(defun LD3Rv8b_POST (_ va_vb_vc xn xm) (LD3Rv._POST va_vb_vc xn 8 64 xm))
(defun LD3Rv16b_POST (_ va_vb_vc xn xm) (LD3Rv._POST va_vb_vc xn 8 128 xm))
(defun LD3Rv4h_POST (_ va_vb_vc xn xm) (LD3Rv._POST va_vb_vc xn 16 64 xm))
(defun LD3Rv8h_POST (_ va_vb_vc xn xm) (LD3Rv._POST va_vb_vc xn 16 128 xm))
(defun LD3Rv2s_POST (_ va_vb_vc xn xm) (LD3Rv._POST va_vb_vc xn 32 64 xm))
(defun LD3Rv4s_POST (_ va_vb_vc xn xm) (LD3Rv._POST va_vb_vc xn 32 128 xm))
(defun LD3Rv1d_POST (_ va_vb_vc xn xm) (LD3Rv._POST va_vb_vc xn 64 64 xm))
(defun LD3Rv2d_POST (_ va_vb_vc xn xm) (LD3Rv._POST va_vb_vc xn 64 128 xm))

;; LD4R (no offset)

(defmacro LD4Rv. (va_vb_vc_vd xn esize dsize)
  (LD.Rv. va_vb_vc_vd xn esize dsize 4))

(defun LD4Rv8b (va_vb_vc_vd xn) (LD4Rv. va_vb_vc_vd xn 8 64))
(defun LD4Rv16b (va_vb_vc_vd xn) (LD4Rv. va_vb_vc_vd xn 8 128))
(defun LD4Rv4h (va_vb_vc_vd xn) (LD4Rv. va_vb_vc_vd xn 16 64))
(defun LD4Rv8h (va_vb_vc_vd xn) (LD4Rv. va_vb_vc_vd xn 16 128))
(defun LD4Rv2s (va_vb_vc_vd xn) (LD4Rv. va_vb_vc_vd xn 32 64))
(defun LD4Rv4s (va_vb_vc_vd xn) (LD4Rv. va_vb_vc_vd xn 32 128))
(defun LD4Rv1d (va_vb_vc_vd xn) (LD4Rv. va_vb_vc_vd xn 64 64))
(defun LD4Rv2d (va_vb_vc_vd xn) (LD4Rv. va_vb_vc_vd xn 64 128))

;; LD4R (post index)

(defmacro LD4Rv._POST (va_vb_vc_vd xn esize dsize off)
  (LD.Rv._POST va_vb_vc_vd xn esize dsize 4 off))

(defun LD4Rv8b_POST (_ va_vb_vc_vd xn xm) (LD4Rv._POST va_vb_vc_vd xn 8 64 xm))
(defun LD4Rv16b_POST (_ va_vb_vc_vd xn xm) (LD4Rv._POST va_vb_vc_vd xn 8 128 xm))
(defun LD4Rv4h_POST (_ va_vb_vc_vd xn xm) (LD4Rv._POST va_vb_vc_vd xn 16 64 xm))
(defun LD4Rv8h_POST (_ va_vb_vc_vd xn xm) (LD4Rv._POST va_vb_vc_vd xn 16 128 xm))
(defun LD4Rv2s_POST (_ va_vb_vc_vd xn xm) (LD4Rv._POST va_vb_vc_vd xn 32 64 xm))
(defun LD4Rv4s_POST (_ va_vb_vc_vd xn xm) (LD4Rv._POST va_vb_vc_vd xn 32 128 xm))
(defun LD4Rv1d_POST (_ va_vb_vc_vd xn xm) (LD4Rv._POST va_vb_vc_vd xn 64 64 xm))
(defun LD4Rv2d_POST (_ va_vb_vc_vd xn xm) (LD4Rv._POST va_vb_vc_vd xn 64 128 xm))

;; LDNP

(defmacro LDNP.i (vn vm base imm size scale)
  "(LDNP.i vn vm base imm) loads a pair of SIMD registers from memory at
   at address base with optional offset imm and stores them in vn and vm.
   Issues a non-temporal hint, in the form of an intrinsic for each memory
   access."
  (let ((off (lshift (cast-signed 64 imm) scale))
        (dbytes (/ size 8)))
    (intrinsic 'non-temporal-hint (+ base off))
    (set$ vn (mem-read (+ base off) dbytes))
    (intrinsic 'non-temporal-hint (+ base off dbytes))
    (set$ vm (mem-read (+ base off dbytes) dbytes))))

(defun LDNPSi (sn sm base imm) (LDNP.i sn sm base imm 32 2))
(defun LDNPDi (dn dm base imm) (LDNP.i dn dm base imm 64 3))
(defun LDNPQi (qn qm base imm) (LDNP.i qn qm base imm 128 4))

;; LDP (pre-index)

(defmacro LDP.pre (vn vm base imm size scale)
  "(LDP.i qn qm imm size mem-load scale) loads a pair of SIMD&FP registers from 
   memory using the address base and an optional signed immediate offset."
  (let ((off (lshift (cast-signed 64 imm) scale))
        (dbytes (/ size 8))
        (addr (+ base off)))
    (set$ vn (mem-read addr (/ size 8)))
    (set$ vm (mem-read (+ addr dbytes) (/ size 8)))
    (set$ base addr)))

(defun LDPQpre (_ qn qm base imm) (LDP.pre qn qm base imm 128 4))
(defun LDPDpre (_ qn qm base imm) (LDP.pre qn qm base imm 64 3))
(defun LDPSpre (_ qn qm base imm) (LDP.pre qn qm base imm 32 2))

;; LDP (post-index)

(defmacro LDP.post (vn vm base imm size scale)
  "(LDP.i qn qm imm size mem-load scale) loads a pair of SIMD&FP registers from 
   memory using the address base and an optional signed immediate offset."
  (let ((off (lshift (cast-signed 64 imm) scale))
        (dbytes (/ size 8)))
    (set$ vn (mem-read base (/ size 8)))
    (set$ vm (mem-read (+ base dbytes) (/ size 8)))
    (set$ base (+ base off))))

(defun LDPQpost (_ qn qm base imm) (LDP.post qn qm base imm 128 4))
(defun LDPDpost (_ qn qm base imm) (LDP.post qn qm base imm 64 3))
(defun LDPSpost (_ qn qm base imm) (LDP.post qn qm base imm 32 2))

;; LDP (signed offset)

(defmacro LDP.i (vn vm base imm size scale)
  "(LDP.i qn qm imm size mem-load scale) loads a pair of SIMD&FP registers from 
   memory using the address base and an optional signed immediate offset."
  (let ((off (lshift (cast-signed 64 imm) scale))
        (dbytes (/ size 8)))
    (set$ vn (mem-read (+ base off) dbytes))
    (set$ vm (mem-read (+ base off dbytes) dbytes))))

(defun LDPQi (qn qm base imm) (LDP.i qn qm base imm 128 4))
(defun LDPDi (qn qm base imm) (LDP.i qn qm base imm 64 3))
(defun LDPSi (qn qm base imm) (LDP.i qn qm base imm 32 2))

;; LDR (immediate, post-index)

(defmacro LDR.post (vt base off size)
  "(LDR.post vt base imm mem-load scale) loads an element from memory from 
   the post-index base address and unsigned immediate offset off and stores the result 
   in vt."
  (prog
    (set$ vt (mem-read base (/ size 8)))
    (set$ base (+ base off))))

(defun LDRBpost (_ bt base imm) (LDR.post bt base imm 8))
(defun LDRHpost (_ ht base imm) (LDR.post ht base imm 16))
(defun LDRSpost (_ st base imm) (LDR.post st base imm 32))
(defun LDRDpost (_ dt base imm) (LDR.post dt base imm 64))
(defun LDRQpost (_ qt base imm) (LDR.post qt base imm 128))

;; LDR (immediate, pre-index)

(defmacro LDR.pre (vt base off size)
  "(LDR.ui vt base imm mem-load scale) loads an element from memory from 
   the pre-index base address and unsigned immediate offset off and stores the result 
   in vt."
  (let ((addr (+ base off)))
    (set$ vt (mem-read addr (/ size 8)))
    (set$ base addr)))

(defun LDRBpre (_ bt base imm) (LDR.pre bt base imm 8))
(defun LDRHpre (_ ht base imm) (LDR.pre ht base imm 16))
(defun LDRSpre (_ st base imm) (LDR.pre st base imm 32))
(defun LDRDpre (_ dt base imm) (LDR.pre dt base imm 64))
(defun LDRQpre (_ qt base imm) (LDR.pre qt base imm 128))

;; LDR (immediate, unsigned offset)

(defmacro LDR.ui (vt base imm size scale)
  "(LDR.ui vt base imm mem-load scale) loads an element from memory from 
   the base address and unsigned immediate offset imm and stores the result 
   in vt."
  (let ((off (lshift (cast-unsigned 64 imm) scale)))
    (set$ vt (mem-read (+ base off) (/ size 8)))))

(defun LDRBui (bt base imm) (LDR.ui bt base imm 8 0))
(defun LDRHui (ht base imm) (LDR.ui ht base imm 16 1))
(defun LDRSui (st base imm) (LDR.ui st base imm 32 2))
(defun LDRDui (dt base imm) (LDR.ui dt base imm 64 3))
(defun LDRQui (qt base imm) (LDR.ui qt base imm 128 4))

;; LDR (literal)

(defmacro LDR.l (vn label bytes)
  "(LDR.l vn label bytes) loads a register from memory at an address 
   relative to the program counter and a program label."
  (let ((off (cast-signed 64 (lshift label 2))))
    (set$ vn (mem-read (+ off (get-program-counter)) bytes))))

(defun LDRSl (sn label) (LDR.l sn label 4))
(defun LDRDl (dn label) (LDR.l dn label 8))
(defun LDRQl (qn label) (LDR.l qn label 16))

;; LDR (register)

(defmacro LDR.ro. (vt base index signed s scale size)
  "(LDR.ro. vt base index signed s scale mem-load) loads a SIMD&FP register 
   from address base and an optionally shifted and extended index."
  (let ((shift (if (= s 1)
          (+ scale 0)
          (+ 0 0)))
        (off (if (= signed 1)
          (cast-signed 64 (lshift index shift))
          (cast-unsigned 64 (lshift index shift)))))
    (set$ vt (mem-read (+ base off) (/ size 8)))))

(defun LDRBroX (bt base index signed s) (LDR.ro. bt base index signed s 0 8))
(defun LDRHroX (ht base index signed s) (LDR.ro. ht base index signed s 1 16))
(defun LDRSroX (st base index signed s) (LDR.ro. st base index signed s 2 32))
(defun LDRDroX (dt base index signed s) (LDR.ro. dt base index signed s 3 64))
(defun LDRQroX (qt base index signed s) (LDR.ro. qt base index signed s 4 128))

(defun LDRBroW (bt base index signed s) (LDR.ro. bt base index signed s 0 8))
(defun LDRHroW (ht base index signed s) (LDR.ro. ht base index signed s 1 16))
(defun LDRSroW (st base index signed s) (LDR.ro. st base index signed s 2 32))
(defun LDRDroW (dt base index signed s) (LDR.ro. dt base index signed s 3 64))
(defun LDRQroW (qt base index signed s) (LDR.ro. qt base index signed s 4 128))

;; LDUR

(defmacro LDUR.i (vt base simm size)
  "(LDUR.i vt base simm mem-load) loads a SIMD&FP register from memory at 
   the address calculated from a base register and optional immediate offset."
  (set$ vt (mem-read (+ base simm) (/ size 8))))

(defun LDURBi (bt base simm) (LDUR.i bt base simm 8))
(defun LDURHi (ht base simm) (LDUR.i ht base simm 16))
(defun LDURSi (st base simm) (LDUR.i st base simm 32))
(defun LDURDi (dt base simm) (LDUR.i dt base simm 64))
(defun LDURQi (qt base simm) (LDUR.i qt base simm 128))

