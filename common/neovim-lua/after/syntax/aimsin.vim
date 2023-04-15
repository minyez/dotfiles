syn match aimsComment	"#.*$"

syn match aimsGeneralOption		"\v<(xc|k_grid|hf_version|empty_states|prodbas_threshold|auxil_basis|partition_acc|use_density_matrix|use_density_matrix_hf|prodbas_threshold|total_energy_method|frozen_core_postscf|spin|relativistic|occupation_type|RI_method|packed_matrix_format|use_full_symmetry|state_lower_limit|KS_method|mixer|n_max_pulay|charge_mix_param|override_illconditioning|elsi_output_matrix|preconditioner|sc_accuracy_rho|sc_accuracy_eev|sc_accuracy_etot|sc_iter_limit|basis_threshold|distributed_spline_storage|restart|restart_save_iterations|relax_geometry|sc_accuracy_forces|default_prodbas_acc)>"
syn match aimsQPEOption	    	"\v<(qpe_calc|anacon_type|freq_grid_type|frequency_points|periodic_gw_optimize(_kgrid_symmetry|_use_gpu|_single_precision|_init)?|periodic_gw_modify_head_with_dielec_func)>"

syn match aimsExtOption	    	"\v<(print_input_librpa)>"

syn match aimsOutputOption	    "output\s\+\v<(basis|self_energy|gw_regular_kgrid|k_eigenvalue|matrices|grids|v_eff|v_hartree|band|dos)>"

syn match aimsBasisOption       "\v<(species|nucleus|mass|l_hartree|cut_pot|basis_dep_cutoff|radial_base|radial_multiplier|angular_grids|division|outer_grid|include_min_basis|pure_gauss|cite_reference|basis_acc)>"
syn match aimsBasisOption       "^\s*\v<(for_aux\s*)?(gaussian|valence|ion_occ|hydro|ionic)>"

" bind to existing syntax groups
hi def link aimsGeneralOption		Identifier
hi def link aimsQPEOption		    Identifier
hi def link aimsExtOption		    Identifier

hi def link aimsOutputOption		Special

hi def link aimsBasisOption	    	String

hi def link aimsComment	        	Comment
