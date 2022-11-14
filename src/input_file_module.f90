      module input_file_module
    
      implicit none

!! file.cio input file 

!! basin
      type input_basin
       character(len=25) :: object_cnt = "object.cnt"
       character(len=25) :: object_prt = "object.prt"
       character(len=25) :: cs_db = "constituents.cs"
      end type input_basin
      type (input_basin) :: in_basin
      	  	  
!! climate
      type input_cli
       character(len=25) :: precip_sta = "precip.dat"
      end type input_cli
      type (input_cli) :: in_cli

!! connect
      type input_con
       character(len=25) :: hru_con = "hru.con"
       character(len=25) :: ru_con = "rout_unit.con"
       character(len=25) :: aqu_con = "aquifer.con"
       character(len=25) :: chan_con = "channel.con"
       character(len=25) :: res_con = "reservoir.con"
       character(len=25) :: rec_con = "exco.con"
       character(len=25) :: out_con = "outlet.con"
      end type input_con
      type (input_con) :: in_con

!! channel
      type input_cha 
       character(len=25) :: cha_dat = "cha.dat"
       character(len=25) :: cha_dr = "cha.dr"
      end type input_cha
      type (input_cha) :: in_cha

!! reservoir
      type input_res
       character(len=25) :: res_dat = "res.dat"
       !character(len=25) :: res_dr = "res.dr"
      end type input_res
      type (input_res) :: in_res

!! routing unit
      type input_ru
       character(len=25) :: ru_def = "rout_unit.def"
       character(len=25) :: ru_ele = "rout_unit.ele"
      end type input_ru
      type (input_ru) :: in_ru

!! HRU
      type input_hru
       character(len=25) :: hru_dat = "hru.dat"
       character(len=25) :: hru_exco = "hru.exco"
       character(len=25) :: hru_wet = "hru.wet"
       character(len=25) :: hru_bmp = "hru.bmp"
       character(len=25) :: hru_dr = "hru.dr"
      end type input_hru
      type (input_hru) :: in_hru
	  	  
!! recall (daily, monthly and annual)
      type input_rec 
       character(len=25) :: recall_rec = "recall.rec"
      end type input_rec
      type (input_rec) :: in_rec

!! aquifer 
      type input_aqu
       character(len=25) :: aqu_dat = "aqu.dat"
      end type input_aqu
      type (input_aqu) :: in_aqu
           
!! landscape units
      type input_lsu
        character(len=25) :: def_lsu = "ls_unit.def"
        character(len=25) :: ele_lsu = "ls_unit.ele"
      end type input_lsu
      type (input_lsu) :: in_lsu
      
!! exco (recall constant)
      type input_exco
       character(len=25) :: exco = "exco.exc"
       character(len=25) :: om = "exco_om.exc"
       character(len=25) :: pest = "exco_pest.exc"
       character(len=25) :: path = "exco_path.exc"
       character(len=25) :: hmet = "exco_hmet.exc"
       character(len=25) :: salt = "exco_salt.exc"
      end type input_exco
      type (input_exco) :: in_exco
	  
!! delivery ratio
      type input_delr
       character(len=25) :: del_ratio = "delratio.del"
	   character(len=25) :: om = "dr_om.del"
	   character(len=25) :: pest = "dr_pest.del"
	   character(len=25) :: path = "dr_path.del"
	   character(len=25) :: hmet = "dr_hmet.del"
	   character(len=25) :: salt = "dr_salt.del"
      end type input_delr
      type (input_delr) :: in_delr

      end module input_file_module 