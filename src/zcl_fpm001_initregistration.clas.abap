class ZCL_FPM001_INITREGISTRATION definition
  public
  final
  create public .

public section.

  interfaces IF_FPM_GUIBB .
  interfaces IF_FPM_GUIBB_FORM .

  types:
    BEGIN OF ty_form ,
        protocol   TYPE char1,
        accept     TYPE char1,
        reject     TYPE char1,
        date       TYPE sy-datum,
      END OF ty_form .
  types:
    BEGIN OF ty_file,
        zfnumb TYPE char1,
        zfname TYPE char1,
      END OF ty_file .
  types:
    ty_t_file TYPE STANDARD TABLE OF ty_file .

  class-data GS_FORM type TY_FORM .
  class-data GT_FILE type TY_T_FILE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_FPM001_INITREGISTRATION IMPLEMENTATION.


  method IF_FPM_GUIBB_FORM~CHECK_CONFIG.
  endmethod.


METHOD IF_FPM_GUIBB_FORM~FLUSH.
  FIELD-SYMBOLS: <fs_value> TYPE any.
  ASSIGN is_data->* TO <fs_value>.

  gs_form = <fs_value>.
ENDMETHOD.


METHOD if_fpm_guibb_form~get_data.

ENDMETHOD.


  method IF_FPM_GUIBB_FORM~GET_DEFAULT_CONFIG.
  endmethod.


METHOD if_fpm_guibb_form~get_definition.

  DATA:lw_field_descr TYPE fpmgb_s_formfield_descr.
  DATA:lt_comp_tab    TYPE abap_component_tab.
  DATA:lw_comp_tab    TYPE abap_componentdescr.
  DATA:lw_action_line TYPE fpmgb_s_actiondef.

  eo_field_catalog ?=  cl_abap_structdescr=>describe_by_data( gs_form ).
  lt_comp_tab = eo_field_catalog->get_components( ).

  LOOP AT lt_comp_tab INTO lw_comp_tab.
    lw_field_descr-name = lw_comp_tab-name.
    lw_field_descr-null_as_blank = abap_true.
*    lw_field_descr-label_by_ddic = abap_true.

    IF lw_field_descr-name = 'PROTOCOL'.
      lw_field_descr-default_display_type = 'FT'.
    ENDIF.

    IF lw_field_descr-name = 'ACCEPT'.
      lw_field_descr-label_text = text-001.
    ENDIF.

    IF lw_field_descr-name = 'BNSLICENSE'.
      lw_field_descr-mandatory = abap_true.
      lw_field_descr-label_text = text-002.
    ENDIF.

    IF lw_field_descr-name = 'DATE'.
      lw_field_descr-read_only = abap_true.
      lw_field_descr-label_text = text-003.
    ENDIF.


    APPEND lw_field_descr TO et_field_description.
    CLEAR lw_field_descr.
  ENDLOOP.


*-----------------------------------------------------------------------
* 设置Preview按钮
*-----------------------------------------------------------------------
  lw_action_line-id       = 'REGISTERED'.
  lw_action_line-visible  = cl_wd_uielement=>e_visible-visible.
  lw_action_line-text     = text-004.
  lw_action_line-tooltip     = text-004.
  lw_action_line-enabled  = abap_false.
  APPEND lw_action_line TO et_action_definition.
  lw_action_line-id       = 'EDIT'.
  lw_action_line-visible  = cl_wd_uielement=>e_visible-visible.
  lw_action_line-text     = text-005.
  lw_action_line-tooltip     = text-005.
  lw_action_line-enabled  = abap_false.
  APPEND lw_action_line TO et_action_definition.
  lw_action_line-id       = 'ACCEPT'.
  lw_action_line-visible  = cl_wd_uielement=>e_visible-visible.
*  lw_action_line-text     = '接受'.
  lw_action_line-enabled  = abap_true.
  APPEND lw_action_line TO et_action_definition.

ENDMETHOD.


METHOD if_fpm_guibb_form~process_event.
ENDMETHOD.


  method IF_FPM_GUIBB~GET_PARAMETER_LIST.
  endmethod.


METHOD IF_FPM_GUIBB~INITIALIZE.

ENDMETHOD.
ENDCLASS.
