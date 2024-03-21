 !subroutine used to get the exe to read files in the dir above SWIFT
    subroutine initialize_paths()
    use input_file_module    
    
    implicit none

    character(len=255) :: main_directory
    character(len=255) :: cwd
    
    character(len=255) :: swift_cal
    
    integer :: diri
        
    call getcwd(cwd)
    diri = INDEX(cwd, '\', back=.TRUE.)
    main_directory = ADJUSTL(cwd(1:diri))
    
    in_basin%object_cnt = trim(main_directory) // "object.cnt"
    in_basin%object_prt = trim(main_directory) // "object.prt"
    in_basin%cs_db = trim(main_directory) // "constituents.cs"
    
    in_cli%precip_sta = trim(main_directory) // "precip.dat"
    
    in_con%hru_con = trim(main_directory) // "hru.con"
    in_con%ru_con = trim(main_directory) // "rout_unit.con"
    in_con%aqu_con = trim(main_directory) // "aquifer.con"
    in_con%chan_con = trim(main_directory) // "chandeg.con"
    in_con%res_con = trim(main_directory) // "reservoir.con"
    in_con%rec_con = trim(main_directory) // "exco.con"
    in_con%out_con = trim(main_directory) // "outlet.con"


    in_ru%ru_def = trim(main_directory) // "rout_unit.def"
    in_ru%ru_ele = trim(main_directory) // "rout_unit.ele"


    in_rec%recall_rec = trim(main_directory) // "recall.rec"



    in_lsu%def_lsu = trim(main_directory) // "ls_unit.def"
    in_lsu%ele_lsu = trim(main_directory) // "ls_unit.ele"


    in_exco%exco = trim(main_directory) // "exco.exc"
    in_exco%om = trim(main_directory) // "exco_om.exc"
    in_exco%pest = trim(main_directory) // "exco_pest.exc"
    in_exco%path = trim(main_directory) // "exco_path.exc"
    in_exco%hmet = trim(main_directory) // "exco_hmet.exc"
    in_exco%salt = trim(main_directory) // "exco_salt.exc"



    in_delr%del_ratio = trim(main_directory) // in_delr%del_ratio
    in_delr%om = trim(main_directory) // in_delr%om
    in_delr%pest = trim(main_directory) // in_delr%pest
    in_delr%path = trim(main_directory) // in_delr%path
    in_delr%hmet = trim(main_directory) // in_delr%hmet
    in_delr%salt = trim(main_directory) // in_delr%salt


    in_swift_obj%prt = trim(main_directory) // "object.prt"
    
    in_swift%cal =trim(main_directory) // "calibration.cal"
    in_swift%cal_parms =trim(main_directory) // "cal_parms.cal"

end subroutine initialize_paths