" This file is generated automatically. Manual edit might be lost
syn match aimsComment	"#.*$"
hi def link aimsComment	Comment

syn match aimsGeometry	"^\s*\v<(atom|atom_frac|constrain_relaxation|lattice_vector)>"
hi def link aimsGeometry	Identifier

syn match aimsGeneral	"^\s*\v<(KS_method|RI_method|anacon_type|auxil_basis|basis_threshold|calculate_fock_matrix_version|calc_spectral_func|charge|charge_mix_param|compensate_multipole_errors|compute_analytical_stress|compute_forces|compute_heat_flux|contour_def_gw|contour_eta|contour_zshot_offset|default_initial_moment|default_max_l_prodbas|default_max_n_prodbas|default_prodbas_acc|distributed_spline_storage|elsi_output_matrix|elsi_restart|empty_states|exx_band_structure_version|freq_grid_type|frequency_points|frozen_core_postscf|full_cmplx_sigma|global_memory_tracking|gw_hedin_shift|gw_zshot|hartree_potential_version|hf_version|hse_unit|include_spin_orbit|iterations_sc_cd|k_grid|k_offset|load_balancing|maximum_frequency|maximum_time|mixer|n_anacon_par|n_max_pulay|nocc_sc_cd|nvirt_sc_cd|occupation_acc|occupation_type|output_level|override_illconditioning|packed_matrix_format|packed_matrix_threshold|partition_acc|periodic_gw_modify_head_with_dielec_func|periodic_gw_optimize|periodic_gw_optimize_init|periodic_gw_optimize_kgrid_symmetry|periodic_gw_optimize_lvl_init|periodic_gw_optimize_single_precision|periodic_gw_optimize_use_gpu|pole_max|pole_min|postprocess_anyway|preconditioner|print_input_librpa|print_self_energy|prodbas_threshold|qpe_calc|qpe_solver|read_sigc_imagfreq_grid|relativistic|relax_geometry|restart|restart_periodic_gw|restart_save_iterations|ri_multipole_threshold|sbtgrid_lnk0|sbtgrid_lnr0|sbtgrid_lnrange|sbtgrid_N|sc_accuracy_eev|sc_accuracy_etot|sc_accuracy_forces|sc_accuracy_rho|sc_init_iter|sc_iter_limit|sc_self_energy|set_blacsdim|spectral_func_state|spin|state_lower_limit|state_upper_limit|symmetry_reduced_k_grid|total_energy_method|try_zshot|use_density_matrix|use_density_matrix_hf|use_full_symmetry|use_local_index|use_logsbt|use_pimd_wrapper|xc)>"
hi def link aimsGeneral	Identifier

syn match aimsOutput	"^\s*output\s\+\v<(band|band_mulliken|basis|batch_statistics|dos|grids|gw_regular_kgrid|h_s_matrices|k_eigenvalue|matrices|memory_tracking|self_energy|v_eff|v_hartree)>"
hi def link aimsOutput	Special

syn match aimsSpecies	"^\s*\v<(angular_grids|aux_gaussian|basis_acc|basis_dep_cutoff|cite_reference|confined|cut_pot|division|gaussian|include_min_basis|ion_occ|l_hartree|mass|nucleus|outer_grid|prodbas_acc|max_l_prodbas|max_n_prodbas|pure_gauss|radial_base|radial_multiplier|species|valence)>"
syn match aimsSpecies	"^\s*\v<(for_aux\s+)?(hydro|ionic)>"
hi def link aimsSpecies	String

