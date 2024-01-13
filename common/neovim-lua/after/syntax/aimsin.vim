" This file is generated automatically. Manual edit might be lost
syn match aimsComment	"#.*$"
hi def link aimsComment	Comment

syn match aimsGeometry	"\v<(atom|atom_frac|constrain_relaxation|lattice_vector)>"
hi def link aimsGeometry	Identifier

syn match aimsGeneral	"\v<(KS_method|RI_method|anacon_type|auxil_basis|basis_threshold|calculate_fock_matrix_version|charge|charge_mix_param|default_initial_moment|default_prodbas_acc|distributed_spline_storage|elsi_output_matrix|elsi_restart|empty_states|exx_band_structure_version|freq_grid_type|frequency_points|frozen_core_postscf|frozen_core_postscf|global_memory_tracking|hartree_potential_version|hf_version|hse_unit|include_spin_orbit|k_grid|k_offset|load_balancing|mixer|n_anacon_par|n_max_pulay|occupation_acc|occupation_type|output_level|override_illconditioning|packed_matrix_format|packed_matrix_threshold|partition_acc|periodic_gw_modify_head_with_dielec_func|periodic_gw_optimize|periodic_gw_optimize_init|periodic_gw_optimize_kgrid_symmetry|periodic_gw_optimize_lvl_init|periodic_gw_optimize_single_precision|periodic_gw_optimize_use_gpu|postprocess_anyway|preconditioner|print_input_librpa|prodbas_threshold|prodbas_threshold|qpe_calc|qpe_solver|read_sigc_imagfreq_grid|relativistic|relax_geometry|restart|restart_periodic_gw|restart_save_iterations|sc_accuracy_eev|sc_accuracy_etot|sc_accuracy_forces|sc_accuracy_rho|sc_init_iter|sc_iter_limit|set_blacsdim|spin|state_lower_limit|state_upper_limit|symmetry_reduced_k_grid|total_energy_method|use_density_matrix|use_density_matrix_hf|use_full_symmetry|use_local_index|use_logsbt|xc)>"
hi def link aimsGeneral	Identifier

syn match aimsOutput	"output\s\+\v<(band|band_mulliken|basis|batch_statistics|dos|grids|gw_regular_kgrid|h_s_matrices|k_eigenvalue|matrices|memory_tracking|self_energy|v_eff|v_hartree)>"
hi def link aimsOutput	Special

syn match aimsSpecies	"\v<(angular_grids|basis_acc|basis_dep_cutoff|cite_reference|cut_pot|division|include_min_basis|l_hartree|mass|nucleus|outer_grid|pure_gauss|radial_base|radial_multiplier|species)>"
syn match aimsSpecies	"^\s*\v<(for_aux\s*)?(gaussian|valence|ion_occ|hydro|ionic|confined)>"
hi def link aimsSpecies	String

