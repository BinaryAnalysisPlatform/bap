(defpackage pcode-x86 (:use pcode))
(in-package pcode-x86)

(require pcode)

(defmacro cpuid-subr (name tr r tx x)
  (set# tr r (intrinsic name
                        (get# tx x)
                        :result tr)))

(defun cpuid (tr r tx x)
  (cpuid-subr 'cpuid tr r tx x))

(defun cpuid_basic_info (tr r tx x)
  (cpuid-subr 'cpuid_basic_info tr r tx x))

(defun cpuid_Version_info (tr r tx x)
  (cpuid-subr 'cpuid_version_info tr r tx x))

(defun cpuid_cache_tlb_info (tr r tx x)
  (cpuid-subr 'cpuid_cache_tlb_info tr r tx x))

(defun cpuid_serial_info (tr r tx x)
  (cpuid-subr 'cpuid_serial_info tr r tx x))

(defun cpuid_Deterministic_Cache_Parameters_info (tr r tx x)
  (cpuid-subr 'cpuid_deterministic_cache_parameters_info tr r tx x))

(defun cpuid_MONITOR_MWAIT_Features_info (tr r tx x)
  (cpuid-subr 'cpuid_monitor_mwait_features_info tr r tx x))

(defun cpuid_Extended_Feature_Enumeration_info (tr r tx x)
  (cpuid-subr 'cpuid_extended_feature_enumeration_info tr r tx x))

(defun cpuid_Direct_Cache_Access_info (tr r tx x)
  (cpuid-subr 'cpuid_direct_cache_access_info tr r tx x))

(defun cpuid_Architectural_Performance_Monitoring_info (tr r tx x)
  (cpuid-subr 'cpuid_architectural_performance_monitoring_info tr r tx x))


(defun cpuid_Extended_Topology_info (tr r tx x)
  (cpuid-subr 'cpuid_extended_topology_info tr r tx x))

(defun cpuid_Processor_Extended_States_info (tr r tx x)
  (cpuid-subr 'cpuid_processor_extended_states_info tr r tx x))

(defun cpuid_Quality_of_Service_info (tr r tx x)
  (cpuid-subr 'cpuid_quality_of_service_info tr r tx x))

(defun cpuid_brand_part1_info (tr r tx x)
  (cpuid-subr 'cpuid_brand_part1_info tr r tx x))

(defun cpuid_brand_part2_info (tr r tx x)
  (cpuid-subr 'cpuid_brand_part2_info tr r tx x))

(defun cpuid_brand_part3_info (tr r tx x)
  (cpuid-subr 'cpuid_brand_part3_info tr r tx x))
