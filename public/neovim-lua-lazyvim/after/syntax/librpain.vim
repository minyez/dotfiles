" This file is generated automatically. Manual edit might be lost
syn match librpaComment	"#.*$"
hi def link librpaComment	Comment

syn match librpaAPI	"^\s*\v<(output_dir|restart_from_dir|nfreq|parallel_routing|vq_threshold|tfgrids_type|tfgrids_freq_min|tfgrids_freq_interval|tfgrids_freq_max|tfgrids_time_min|tfgrids_time_interval|minimax_emin|minimax_emax|minimax_regulation|n_bands_chi0|n_bands_sigc|option_bvk_remap|gf_threshold|libri_chi0_collect_s0_chunk|libri_chi0_collect_max_bytes|libri_chi0_threshold_C|libri_chi0_threshold_G|libri_exx_threshold_C|libri_exx_threshold_D|libri_exx_threshold_V|n_params_anacon|n_params_anacon_resample|anacon_tfgrids_type|anacon_nfreq|sqrt_coulomb_threshold|libri_g0w0_threshold_C|libri_g0w0_threshold_G|libri_g0w0_threshold_Wc|option_dielect_func|rpa_headwing_body_start|ifreq_output_wc_start|ifreq_output_wc_end|option_qpe_solver|qpe_solver_n_iter_max|qpe_solver_thres|qpe_solver_damp_factor|sf_gf_omega_shift|sf_sigc_omega_shift)>"
hi def link librpaAPI	Identifier

syn match librpaAPISW	"^\s*\v<(use_kpara_scf_eigvec|override_qpe_solver_nan|use_qpe_adaptive_damp|use_qpe_legacy_update|use_fullcoul_eps|use_fullcoul_exx|use_fullcoul_wc|use_symmetry_exx|use_symmetry_rpa|use_symmetry_gw|output_abacus_gw_gf|output_gw_sigc_ks_kf|output_gw_sigc_ks_mat_kf|output_gw_sigc_mat_rt|output_gw_sigc_mat_rf|output_wc_rf|replace_w_head|read_sigc_mat_rf|use_shrink_abfs|use_shrink_chi|use_scalapack_ecrpa|use_2d_dielectric|use_scalapack_gw_wc|use_cholesky_gw_wc|use_gpu_replace_scalapack|use_elpa_sqrt_coulomb)>"
hi def link librpaAPISW	Identifier

syn match librpaDRIVER	"^\s*\v<(output_level|task|constants_choice|input_dir|cs_threshold|i_state_low|i_state_high|use_pyatb|use_spinor_wfc|prefix_lri_coeff|prefix_lri_coeff_shrink|prefix_shrink_sinvS|prefix_coul_full|prefix_coul_cut|prefix_eigvecs_scf|fn_stru|fn_basis|fn_basis_wfc|fn_basis_aux|fn_basis_aux_shrink|fn_bz_sampling|fn_eigocc_scf|fn_dielfunc|fn_vxc_scf|fn_band_kpath_info|version_coul_reader|version_lri_reader|sf_omega_start|sf_omega_end|sf_omega_step|sf_state_start|sf_state_end)>"
hi def link librpaDRIVER	Special

syn match librpaDRIVERSW	"^\s*\v<(output_energy_qp|output_hamgnn|output_gw_spec_func)>"
hi def link librpaDRIVERSW	Special

syn match librpaDEVOPTS	"^\s*\v<(use_chi0_q_uhap_split)>"
hi def link librpaDEVOPTS	MoreMsg

syn match librpaDEPRECATED	"^\s*\v<(debug|tfgrid_type|use_soc|fn_basis_shrink|gf_R_threshold|output_gw_sigc_ks_if|output_gw_sigc_mat|option_output_Wc_Rf_mat)>"
hi def link librpaDEPRECATED	ErrorMsg

