syn match aimsComment	"#.*$"

syn match aimsGeneralOption		"\v<(xc|k_grid|symmetry_reduced_k_grid|k_offset|empty_states|partition_acc|use_density_matrix|total_energy_method|spin|occupation_acc|occupation_type|packed_matrix_format|use_full_symmetry|KS_method|mixer|n_max_pulay|charge_mix_param|override_illconditioning|elsi_output_matrix|preconditioner|sc_accuracy_rho|sc_accuracy_eev|sc_accuracy_etot|sc_iter_limit|sc_init_iter|basis_threshold|distributed_spline_storage|restart|restart_save_iterations|restart_periodic_gw|relax_geometry|sc_accuracy_forces|hartree_potential_version|use_local_index|load_balancing|packed_matrix_threshold|use_logsbt|charge|default_initial_moment|hse_unit|frozen_core_postSCF)>"
syn match aimsRelaOption        "\v<include_spin_orbit|relativistic>"
syn match aimsBeyondDFTOption   "\v<calculate_fock_matrix_version|hf_version|prodbas_threshold|auxil_basis|use_density_matrix_hf|prodbas_threshold|frozen_core_postscf|RI_method|state_upper_limit|state_lower_limit|default_prodbas_acc|set_blacsdim +(true|false)>"
syn match aimsDebugOption       "\v<global_memory_tracking|output_level|postprocess_anyway>"
syn match aimsEXXOption	    	"\v<(exx_band_structure_version)>"
syn match aimsQPEOption	    	"\v<(qpe_calc|qpe_solver|anacon_type|n_anacon_par|freq_grid_type|frequency_points|periodic_gw_optimize(_kgrid_symmetry|_use_gpu|_single_precision|_init|_lvl_init)?|periodic_gw_modify_head_with_dielec_func)>"

syn match aimsELSIOption	    	"\v<(elsi_(restart))>"

syn match aimsExtOption	    	"\v<(print_input_librpa|read_sigc_imagfreq_grid)>"

syn match aimsOutputOption	    "output\s\+\v<(basis|self_energy|gw_regular_kgrid|k_eigenvalue|matrices|grids|v_eff|v_hartree|band|band_mulliken|dos|memory_tracking|batch_statistics|h_s_matrices)>"

syn match aimsBasisOption       "\v<(species|nucleus|mass|l_hartree|cut_pot|basis_dep_cutoff|radial_base|radial_multiplier|angular_grids|division|outer_grid|include_min_basis|pure_gauss|cite_reference|basis_acc)>"
syn match aimsBasisOption       "^\s*\v<(for_aux\s*)?(gaussian|valence|ion_occ|hydro|ionic)>"

" bind to existing syntax groups
hi def link aimsGeneralOption		Identifier
hi def link aimsRelaOption  		Identifier
hi def link aimsBeyondDFTOption		Identifier
hi def link aimsDebugOption	    	Identifier
hi def link aimsQPEOption		    Identifier
hi def link aimsEXXOption		    Identifier
hi def link aimsELSIOption		    Identifier
hi def link aimsExtOption		    Identifier

hi def link aimsOutputOption		Special

hi def link aimsBasisOption	    	String

hi def link aimsComment	        	Comment
