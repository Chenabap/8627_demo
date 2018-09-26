CLASS zcl_fpm020_spinfo_change DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_fpm_guibb .
    INTERFACES if_fpm_guibb_search .
    INTERFACES if_fpm_guibb_list .

    TYPES:
      BEGIN OF ty_search,
        partner   TYPE but000-partner,
        name_org1 TYPE but000-name_org1,
        status    TYPE ztvenalter-status,
      END OF ty_search .
    TYPES:
      BEGIN OF ty_list,
        alternum   TYPE ztvenalter-alternum,
        name_org1  TYPE but000-name_org1,
        formalcode TYPE ztvenalter-formalcode,
        altertype  TYPE ztvenalter-altertype,
        status     TYPE ztvenalter-status,
        applydate  TYPE ztvenalter-applydate,
        url        TYPE ztvenalter-url,
      END OF ty_list .
    TYPES:
      ty_t_list TYPE STANDARD TABLE OF ty_list .

    DATA gt_list TYPE ty_t_list .
    DATA gv_is_partner TYPE char1 .
    CLASS-DATA mo_shared_data TYPE REF TO y_cl_wd_shared_data .
    DATA gv_partner TYPE but000-partner .

    METHODS m_excute_search
      IMPORTING
        !it_criteria TYPE fpmgb_t_search_criteria
      EXPORTING
        !et_result   TYPE ty_t_list .
    METHODS m_check_create_alter
      IMPORTING
        !it_criteria TYPE fpmgb_t_search_criteria
        !event_id    TYPE fpm_event_id
      EXPORTING
        !et_messages TYPE fpmgb_search_t_t100_message .
    CLASS-METHODS m_get_alternum
      EXPORTING
        !ev_alternum TYPE zalternum .
protected section.
private section.
ENDCLASS.



CLASS ZCL_FPM020_SPINFO_CHANGE IMPLEMENTATION.


  method IF_FPM_GUIBB_LIST~CHECK_CONFIG.
  endmethod.


METHOD if_fpm_guibb_list~flush.
  FIELD-SYMBOLS: <fs_value> TYPE any.
  ASSIGN it_data->* TO <fs_value>.
  gt_list = <fs_value>.

ENDMETHOD.


METHOD if_fpm_guibb_list~get_data.

  CASE iv_eventid->mv_event_id.
*   初始状态
    WHEN cl_fpm_event=>gc_event_start.
      IF gv_is_partner = 'X'.
        TRY.
            ct_field_usage[ name = 'URL' ]-visibility = '01'.
          CATCH  cx_sy_itab_line_not_found .
        ENDTRY.
        ev_field_usage_changed = abap_true.

      ELSE.
        TRY.
            ct_action_usage[ id = 'GO_TO_QUESTIONNAIRE' ]-visible = '01'.
          CATCH  cx_sy_itab_line_not_found .
        ENDTRY.
        ev_action_usage_changed = abap_true.
      ENDIF.

    WHEN OTHERS.
  ENDCASE.

  ct_data = gt_list.
  ev_data_changed = abap_true.
ENDMETHOD.


  method IF_FPM_GUIBB_LIST~GET_DEFAULT_CONFIG.
  endmethod.


METHOD if_fpm_guibb_list~get_definition.
  DATA: lo_struct_descr TYPE REF TO cl_abap_structdescr,
        ls_field_desc   TYPE fpmgb_s_listfield_descr,
        ls_param        TYPE fpm_s_ext_navigation_info_add5.
  DATA lw_action_line TYPE fpmgb_s_actiondef.
  DATA lt_namevalue TYPE fpmgb_t_namevalue.
  DATA lw_namevalue LIKE LINE OF lt_namevalue.
  DATA lt_comp_tab TYPE abap_component_tab.
  DATA lw_comp_tab TYPE abap_componentdescr.
  DATA lw_field_descr TYPE fpmgb_s_listfield_descr.


  FIELD-SYMBOLS:
    <ls_action_def> TYPE fpmgb_s_actiondef.

  lo_struct_descr ?= cl_abap_structdescr=>describe_by_name( 'TY_LIST' ).
  eo_field_catalog = cl_abap_tabledescr=>create( lo_struct_descr ).
  lt_comp_tab = lo_struct_descr->get_components( ).



*展示字段
  LOOP AT lt_comp_tab INTO lw_comp_tab WHERE name IS NOT INITIAL.
    lw_field_descr-name = lw_comp_tab-name.
    lw_field_descr-allow_sort = abap_true.
    lw_field_descr-allow_filter = abap_true.
    IF lw_field_descr-name = 'ALTERTYPE' OR
        lw_field_descr-name = 'STATUS'.
      lw_field_descr-read_only = abap_true.
    ENDIF.

    CASE lw_field_descr-name.
      WHEN 'FORMALCODE'.
        lw_field_descr-header_label = text-001.
      WHEN 'NAME_ORG1'.
        lw_field_descr-header_label = text-002.
      WHEN 'URL'.
        lw_field_descr-header_label = text-006.
      WHEN OTHERS.
    ENDCASE.
    APPEND lw_field_descr TO et_field_description.
    CLEAR lw_field_descr.
  ENDLOOP.

*-----------------------------------------------------------------------
* 设置按钮
*-----------------------------------------------------------------------
  CLEAR lw_action_line.
  lw_action_line-id       = 'CHANGED_BASIC'.
  lw_action_line-visible  = cl_wd_uielement=>e_visible-visible.
  lw_action_line-text     = text-003.
  lw_action_line-tooltip  = text-003.
  lw_action_line-enabled  = abap_true.
  APPEND lw_action_line TO et_action_definition.
  CLEAR lw_action_line.
  lw_action_line-id       = 'CHANGED_COTH'.
  lw_action_line-visible  = cl_wd_uielement=>e_visible-visible.
  lw_action_line-text     = text-004.
  lw_action_line-tooltip  = text-004.
  lw_action_line-enabled  = abap_true.
  APPEND lw_action_line TO et_action_definition.
  CLEAR lw_action_line.
  lw_action_line-id       = 'CHANGED_FILE'.
  lw_action_line-visible  = cl_wd_uielement=>e_visible-visible.
  lw_action_line-text     = text-005.
  lw_action_line-tooltip  = text-005.
  lw_action_line-enabled  = abap_true.
  APPEND lw_action_line TO et_action_definition.
  CLEAR lw_action_line.
  lw_action_line-id       = 'GO_TO_QUESTIONNAIRE'.
  lw_action_line-visible  = cl_wd_uielement=>e_visible-visible.
  lw_action_line-text     = text-007.
  lw_action_line-tooltip  = text-007.
  lw_action_line-enabled  = abap_true.
  APPEND lw_action_line TO et_action_definition.
  CLEAR lw_action_line.
  lw_action_line-id       = 'GO_TO_ALTERPAGE'.
  lw_action_line-visible  = cl_wd_uielement=>e_visible-visible.
  lw_action_line-enabled  = abap_true.
  APPEND lw_action_line TO et_action_definition.

ENDMETHOD.


METHOD if_fpm_guibb_list~process_event.
  DATA: lw_param TYPE fpm_s_dialog_box_properties.


  CASE io_event->mv_event_id.
**1 基本信息维护申请
*    WHEN 'CHANGED_BASIC'.
*      mo_shared_data->if_fpm_parameter~set_value( iv_key   = 'IS_PARTNER' iv_value = gv_is_partner ).
*
*      lw_param-height = '800'.
*      lw_param-width = '1200'.
*      lw_param-text_for_close_button = 'Close'.
*      y_cl_wd_tool=>open_dialog( iv_param = lw_param iv_page = 'CHANGE_BASIC' ).
*
**2 联系信息维护
*    WHEN 'CHANGED_COTH'.
*      mo_shared_data->if_fpm_parameter~set_value( iv_key   = 'IS_PARTNER' iv_value = gv_is_partner ).
*
*      lw_param-height = '200'.
*      lw_param-width = '1200'.
*      lw_param-text_for_close_button = 'Close'.
*      y_cl_wd_tool=>open_dialog( iv_param = lw_param iv_page = 'CHANGE_COTH' ).
*
**3 体系资质文件维护
*    WHEN 'CHANGED_FILE'.
*      mo_shared_data->if_fpm_parameter~set_value( iv_key   = 'IS_PARTNER' iv_value = gv_is_partner ).
*
*      lw_param-height = '200'.
*      lw_param-width = '1600'.
*      lw_param-text_for_close_button = 'Close'.
*      y_cl_wd_tool=>open_dialog( iv_param = lw_param iv_page = 'CHANGE_FILE' ).


*4 跳转变更页面
    WHEN 'GO_TO_ALTERPAGE'.
* 4.1 基本信息维护
      IF gt_list[ iv_event_index ]-altertype = '01'.
        mo_shared_data->if_fpm_parameter~set_value( iv_key   = 'IS_PARTNER' iv_value = gv_is_partner ).
        mo_shared_data->if_fpm_parameter~set_value( iv_key   = 'PARTNER' iv_value = gt_list[ iv_event_index ]-formalcode ).
        mo_shared_data->if_fpm_parameter~set_value( iv_key   = 'ALTERNUM' iv_value = gt_list[ iv_event_index ]-alternum ).
        lw_param-height = '800'.
        lw_param-width = '1200'.
        lw_param-title = text-003.
        y_cl_wd_tool=>open_dialog( iv_param = lw_param iv_page = 'CHANGE_BASIC' ).

* 4.2 体系资质附件维护
      ELSEIF gt_list[ iv_event_index ]-altertype = '02'.
        mo_shared_data->if_fpm_parameter~set_value( iv_key   = 'IS_PARTNER' iv_value = gv_is_partner ).
        mo_shared_data->if_fpm_parameter~set_value( iv_key   = 'PARTNER' iv_value = gt_list[ iv_event_index ]-formalcode ).
        mo_shared_data->if_fpm_parameter~set_value( iv_key   = 'ALTERNUM' iv_value = gt_list[ iv_event_index ]-alternum ).
        lw_param-height = '200'.
        lw_param-width = '1600'.
        lw_param-title = text-005.
        y_cl_wd_tool=>open_dialog( iv_param = lw_param iv_page = 'CHANGE_FILE' ).

* 4.3 联系信息维护
      ELSEIF gt_list[ iv_event_index ]-altertype = '03'.
        mo_shared_data->if_fpm_parameter~set_value( iv_key   = 'IS_PARTNER' iv_value = gv_is_partner ).
        mo_shared_data->if_fpm_parameter~set_value( iv_key   = 'PARTNER' iv_value = gt_list[ iv_event_index ]-formalcode ).
        mo_shared_data->if_fpm_parameter~set_value( iv_key   = 'ALTERNUM' iv_value = gt_list[ iv_event_index ]-alternum ).

        lw_param-height = '200'.
        lw_param-width = '1200'.
        lw_param-title = text-004.
        y_cl_wd_tool=>open_dialog( iv_param = lw_param iv_page = 'CHANGE_COTH' ).
      ENDIF.

*5 跳转到调查问卷
    WHEN 'GO_TO_QUESTIONNAIRE'.
      SELECT SINGLE partnernum INTO @DATA(lv_partnernum) FROM ztvenstatus WHERE formalcode = @gv_partner.
      IF lv_partnernum IS INITIAL.
        SELECT SINGLE partnernum INTO lv_partnernum FROM ztvenstatus WHERE formalcode = gv_partner.
      ENDIF.
      CALL METHOD zcl_fpm001_initregistration=>m_goto_questionnaire
        EXPORTING
          partnernum        = lv_partnernum
          from_preselection = 'X'.

    WHEN OTHERS.
  ENDCASE.
ENDMETHOD.


  method IF_FPM_GUIBB_SEARCH~CHECK_CONFIG.
  endmethod.


method IF_FPM_GUIBB_SEARCH~FLUSH.
endmethod.


METHOD if_fpm_guibb_search~get_data.
  DATA: lw_orgunit     TYPE hrs_objec,
        lw_criteria    TYPE fpmgb_s_search_criteria,
        lv_partner     TYPE but000-partner,
        lw_field_usage TYPE fpmgb_s_search_fieldusage.

  CASE io_event->mv_event_id.
    WHEN cl_fpm_event=>gc_event_start.
*1 供应商账号
      CALL FUNCTION 'BBP_OM_STRUC_GET_ORG_FROM_USER'
        EXPORTING
          user                    = sy-uname
        IMPORTING
          orgunit                 = lw_orgunit
        EXCEPTIONS
          path_not_found          = 1
          error_reading_structure = 2
          no_roots                = 3
          invalid_roots           = 4
          internal_error          = 5
          user_not_assigned       = 6
          OTHERS                  = 7.
      IF lw_orgunit-objid IS INITIAL.
        SELECT SINGLE formalcode INTO lv_partner
          FROM ztvenstatus
         WHERE formalacc = sy-uname.

        LOOP AT ct_fpm_search_criteria INTO lw_criteria.
          IF lw_criteria-search_attribute = 'PARTNER'.
            lw_criteria-low = lv_partner.
          ENDIF.

          IF lw_criteria-search_attribute = 'NAME_ORG1'.
            SELECT SINGLE name_org1 INTO lw_criteria-low
              FROM but000
             WHERE partner = lv_partner.
          ENDIF.

          MODIFY ct_fpm_search_criteria FROM lw_criteria.
        ENDLOOP.

        LOOP AT ct_field_usage INTO lw_field_usage WHERE name = 'PARTNER' OR name = 'NAME_ORG1'.
          lw_field_usage-read_only = abap_true.
          MODIFY ct_field_usage FROM lw_field_usage.
        ENDLOOP.

        ev_field_usage_changed = abap_true.
        ev_search_criteria_changed = abap_true.
      ENDIF.


    WHEN OTHERS.
  ENDCASE.
ENDMETHOD.


  method IF_FPM_GUIBB_SEARCH~GET_DEFAULT_CONFIG.
  endmethod.


METHOD if_fpm_guibb_search~get_definition.
  DATA: lo_structdescr TYPE REF TO cl_abap_typedescr,
        lw_components  TYPE abap_compdescr,
        ls_field_descr TYPE fpmgb_s_searchfield_descr.

  lo_structdescr = cl_abap_structdescr=>describe_by_name('TY_SEARCH').
  eo_field_catalog_attr ?= lo_structdescr.

  LOOP AT eo_field_catalog_attr->components INTO lw_components.
    ls_field_descr-name           = lw_components-name.
    ls_field_descr-deactivate_value_help = abap_true.
    CASE ls_field_descr-name.
*      审批状态
      WHEN 'STATUS'.
        ls_field_descr-is_of_type = 'EN'.

      WHEN 'PARTNER'.
        ls_field_descr-text = TEXT-001.
      WHEN 'NAME_ORG1'.
        ls_field_descr-text = text-002.

      WHEN OTHERS.
    ENDCASE.
    APPEND ls_field_descr TO et_field_description_attr.
    CLEAR ls_field_descr.
  ENDLOOP.

ENDMETHOD.


METHOD if_fpm_guibb_search~process_event.
  DATA: lw_param TYPE fpm_s_dialog_box_properties.


*  检查供应商编号
  IF io_event->mv_event_id = 'CHANGED_BASIC' OR
     io_event->mv_event_id = 'CHANGED_COTH' OR
     io_event->mv_event_id = 'CHANGED_FILE' OR
     io_event->mv_event_id = 'GO_TO_QUESTIONNAIRE'.
    CALL METHOD me->m_check_create_alter
      EXPORTING
        it_criteria = it_fpm_search_criteria
        event_id    = io_event->mv_event_id
      IMPORTING
        et_messages = et_messages.
    READ TABLE et_messages TRANSPORTING NO FIELDS
      WITH KEY severity = 'E'.
    IF sy-subrc = 0.
      RETURN.
    ENDIF.

    IF io_event->mv_event_id = 'CHANGED_BASIC' OR
       io_event->mv_event_id = 'CHANGED_COTH' OR
       io_event->mv_event_id = 'CHANGED_FILE'.
      mo_shared_data->if_fpm_parameter~set_value( iv_key   = 'PARTNER' iv_value = gv_partner ).
    ENDIF.
  ENDIF.


  CASE io_event->mv_event_id.
    WHEN if_fpm_guibb_search=>fpm_execute_search.
      CALL METHOD me->m_excute_search
        EXPORTING
          it_criteria = it_fpm_search_criteria
        IMPORTING
          et_result   = gt_list.

*1 基本信息维护申请
    WHEN 'CHANGED_BASIC'.
      mo_shared_data->if_fpm_parameter~set_value( iv_key   = 'IS_PARTNER' iv_value = gv_is_partner ).

      lw_param-height = '800'.
      lw_param-width = '1200'.
      lw_param-title = text-003.
      lw_param-text_for_close_button = 'Close'.
      y_cl_wd_tool=>open_dialog( iv_param = lw_param iv_page = 'CHANGE_BASIC' ).

*2 联系信息维护
    WHEN 'CHANGED_COTH'.
      mo_shared_data->if_fpm_parameter~set_value( iv_key   = 'IS_PARTNER' iv_value = gv_is_partner ).

      lw_param-height = '200'.
      lw_param-width = '1200'.
      lw_param-title = text-004.
      lw_param-text_for_close_button = 'Close'.
      y_cl_wd_tool=>open_dialog( iv_param = lw_param iv_page = 'CHANGE_COTH' ).

*3 体系资质文件维护
    WHEN 'CHANGED_FILE'.
      mo_shared_data->if_fpm_parameter~set_value( iv_key   = 'IS_PARTNER' iv_value = gv_is_partner ).

      lw_param-height = '200'.
      lw_param-width = '1600'.
      lw_param-title = text-005.
      lw_param-text_for_close_button = 'Close'.
      y_cl_wd_tool=>open_dialog( iv_param = lw_param iv_page = 'CHANGE_FILE' ).
    WHEN OTHERS.
  ENDCASE.
ENDMETHOD.


  method IF_FPM_GUIBB~GET_PARAMETER_LIST.
  endmethod.


METHOD if_fpm_guibb~initialize.
  DATA: lt_criteria TYPE fpmgb_t_search_criteria,
        lw_criteria TYPE fpmgb_s_search_criteria,
        lw_orgunit  TYPE hrs_objec.

  CALL FUNCTION 'BBP_OM_STRUC_GET_ORG_FROM_USER'
    EXPORTING
      user                    = sy-uname
    IMPORTING
      orgunit                 = lw_orgunit
    EXCEPTIONS
      path_not_found          = 1
      error_reading_structure = 2
      no_roots                = 3
      invalid_roots           = 4
      internal_error          = 5
      user_not_assigned       = 6
      OTHERS                  = 7.
  IF lw_orgunit-objid IS INITIAL.
    gv_is_partner = 'X'.
  ENDIF.


  SELECT SINGLE formalcode INTO lw_criteria-low
    FROM ztvenstatus
   WHERE formalacc = sy-uname.
  lw_criteria-search_attribute = 'PARTNER'.
  lw_criteria-operator = '01'.
  lw_criteria-sign = 'I'.
  lw_criteria-is_initial_value_evaluated = 'X'.
  APPEND lw_criteria TO lt_criteria.

  CALL METHOD me->m_excute_search
    EXPORTING
      it_criteria = lt_criteria
    IMPORTING
      et_result   = gt_list.

*  实例化共享类
  mo_shared_data ?= y_cl_wd_shared_data=>get_instance( ).

ENDMETHOD.


METHOD m_check_create_alter.
  DATA: lt_criteria TYPE fpmgb_t_search_criteria,
        lw_criteria TYPE fpmgb_s_search_criteria,
        lw_message  TYPE fpmgb_search_s_t100_message,
        lv_counter  TYPE i.
  DATA: lv_altertype TYPE ztvenalter-altertype,
        lv_alternum  TYPE ztvenalter-alternum,
        lv_operation TYPE ztvenalter-operation.

  lt_criteria = it_criteria.
  DELETE lt_criteria WHERE search_attribute <> 'PARTNER'.
  DESCRIBE TABLE lt_criteria LINES lv_counter.
  IF lv_counter <> 1.
    CLEAR:lw_message.
    lw_message-msgid = 'ZSRM'.
    lw_message-msgno = '111'.
    lw_message-severity = 'E'.
    APPEND lw_message TO et_messages.
    RETURN.
  ENDIF.

  READ TABLE lt_criteria INTO lw_criteria INDEX 1.
  IF sy-subrc = 0.
    IF lw_criteria-operator <> '01' OR
       lw_criteria-low IS INITIAL.
      CLEAR:lw_message.
      lw_message-msgid = 'ZSRM'.
      lw_message-msgno = '111'.
      lw_message-severity = 'E'.
      APPEND lw_message TO et_messages.
      RETURN.
    ENDIF.
  ENDIF.

  SELECT COUNT(*)
    FROM but000
   WHERE partner = lw_criteria-low.
  IF sy-subrc <> 0.
    CLEAR:lw_message.
    lw_message-msgid = 'ZSRM'.
    lw_message-msgno = '073'.
    lw_message-severity = 'E'.
    APPEND lw_message TO et_messages.
    RETURN.
  ENDIF.

  IF event_id = 'CHANGED_BASIC'.
    lv_altertype = '01'.
    lv_operation = '02'.
  ELSEIF event_id = 'CHANGED_COTH'.
    lv_altertype = '03'.
  ELSEIF event_id = 'CHANGED_FILE'.
    lv_altertype = '02'.
  ENDIF.
  IF event_id = 'CHANGED_COTH' OR event_id = 'CHANGED_FILE' OR ( event_id = 'CHANGED_BASIC' AND gv_is_partner = 'X' ).
    SELECT SINGLE alternum INTO lv_alternum
      FROM ztvenalter
     WHERE formalcode = lw_criteria-low
       AND altertype = lv_altertype
       AND operation = lv_operation
       AND ( status = '01' OR
             status = '02' OR
             status = '03' OR
             status = '04' OR
             status = '05' OR
             status = '06' OR
             status = '08' ).
    IF sy-subrc = 0.
      CLEAR:lw_message.
      lw_message-msgid = 'ZSRM'.
      lw_message-msgno = '125'.
      lw_message-severity = 'E'.
      lw_message-parameter_1 = lv_alternum.
      APPEND lw_message TO et_messages.
      RETURN.
    ENDIF.
  ENDIF.

  gv_partner = lw_criteria-low.
ENDMETHOD.


METHOD m_excute_search.
  DATA: lt_criteria  TYPE fpmgb_t_search_criteria,
        lw_sel       TYPE rsdsselopt,
        lv_fieldname TYPE string.
  FIELD-SYMBOLS: <fs_criteria> TYPE fpmgb_s_search_criteria,
                 <fs_sel>      TYPE STANDARD TABLE.

  DATA: lw_result   TYPE ty_list.
  DATA: lt_but000 TYPE STANDARD TABLE OF but000,
        lw_but000 TYPE but000.

*--------------------------------------------------------------------*
*  Define ranges table of select field
*--------------------------------------------------------------------*
  DATA: r_partner   TYPE STANDARD TABLE OF rsdsselopt,
        r_name_org1 TYPE STANDARD TABLE OF rsdsselopt,
        r_status    TYPE STANDARD TABLE OF rsdsselopt.

*--------------------------------------------------------------------*
*  Construct ranges tables
*--------------------------------------------------------------------*
  lt_criteria = it_criteria.
  SORT lt_criteria BY search_attribute.
  LOOP AT lt_criteria ASSIGNING <fs_criteria>.
    AT NEW search_attribute.
      lv_fieldname = 'R_' && <fs_criteria>-search_attribute.
      ASSIGN (lv_fieldname) TO <fs_sel>.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
    ENDAT.

    TRY.
        CALL METHOD cl_fpm_guibb_search_conversion=>to_abap_select_option
          EXPORTING
            is_fpm_search_row  = <fs_criteria>
          RECEIVING
            rs_abap_sel_option = lw_sel.

        IF lw_sel-low IS NOT INITIAL OR lw_sel-high IS NOT INITIAL.
          APPEND lw_sel TO <fs_sel>.
        ENDIF.
        CLEAR: lw_sel.
      CATCH cx_fpmgb.
    ENDTRY.
  ENDLOOP.

*--------------------------------------------------------------------*
*  Excute SQL
*--------------------------------------------------------------------*
  IF r_partner[] IS INITIAL AND gv_is_partner = 'X'.
    EXIT.
  ENDIF.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_but000
    FROM but000
   WHERE partner IN r_partner
     AND name_org1 IN r_name_org1.

  IF lt_but000 IS INITIAL AND
     ( r_partner[] IS NOT INITIAL OR
       r_name_org1[] IS NOT INITIAL ).
    RETURN.
  ENDIF.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE et_result
    FROM ztvenalter
     FOR ALL ENTRIES IN lt_but000
   WHERE formalcode = lt_but000-partner
     AND status IN r_status.

  LOOP AT et_result INTO lw_result.
    READ TABLE lt_but000 INTO lw_but000
      WITH KEY partner = lw_result-formalcode BINARY SEARCH.
    IF sy-subrc = 0.
      lw_result-name_org1 = lw_but000-name_org1.
    ENDIF.

    MODIFY et_result FROM lw_result.
  ENDLOOP.




ENDMETHOD.


METHOD m_get_alternum.
  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = '01'                "这个就是维护的间隔号
      object                  = 'ZALTERNUM'           "这个就是流水号对象
    IMPORTING
      number                  = ev_alternum            "获得的流水号
*     quantity                = quant
*     returncode              = code
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      OTHERS                  = 8.

ENDMETHOD.
ENDCLASS.
