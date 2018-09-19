class ZCL_FPM002_BASIC_02 definition
  public
  final
  create public .

public section.

  interfaces IF_FPM_GUIBB .
  interfaces IF_FPM_GUIBB_LIST .

  types:
    BEGIN OF ty_list,
        partnernum   TYPE ztvenprd-partnernum,
        topclass     TYPE ztvenprd-topclass,
        bottomclass  TYPE ztvenprd-bottomclass,
        notes        TYPE ztvenprd-notes,
        bottomclassd TYPE ztventopur-bottomclassd,
      END OF ty_list .
  types:
    ty_t_list TYPE STANDARD TABLE OF ty_list .

  data GT_LIST type TY_T_LIST .
  data GV_PARTNERNUM type ZPARTNERNUM .
  class-data MO_SHARED_DATA type ref to Y_CL_WD_SHARED_DATA .
  data GV_FROM_PRESELECTION type XFELD .
protected section.
private section.
ENDCLASS.



CLASS ZCL_FPM002_BASIC_02 IMPLEMENTATION.


  method IF_FPM_GUIBB_LIST~CHECK_CONFIG.
  endmethod.


  METHOD if_fpm_guibb_list~flush.
    FIELD-SYMBOLS: <fs_value> TYPE any.
    ASSIGN it_data->* TO <fs_value>.
    gt_list = <fs_value>.
  ENDMETHOD.


METHOD if_fpm_guibb_list~get_data.
  DATA: lv_tab_lines TYPE i.
  CASE iv_eventid->mv_event_id.
*    初始状态
    WHEN cl_fpm_event=>gc_event_start.
      SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_list
        FROM ztvenprd
        JOIN ztventopur
          ON ztvenprd~bottomclass = ztventopur~bottomclass
       WHERE partnernum = gv_partnernum.

**2 从其他步骤转入
*    WHEN cl_fpm_event=>gc_event_previous_step.
*      SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_list
*        FROM ztvenprd
*        JOIN ztventopur
*          ON ztvenprd~topclass = ztventopur~topclass
*         AND ztvenprd~bottomclass = ztventopur~bottomclass
*       WHERE partnernum = gv_partnernum.

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
    lw_field_descr-header_label_by_ddic = abap_true.

    IF lw_field_descr-name = 'TOPCLASS' OR
       lw_field_descr-name = 'BOTTOMCLASS'.
      lw_field_descr-mandatory = abap_true.
    ENDIF.

    IF gv_from_preselection = 'X'.
      lw_field_descr-read_only = abap_true.
    ENDIF.
    APPEND lw_field_descr TO et_field_description.
    CLEAR lw_field_descr.

  ENDLOOP.

*-----------------------------------------------------------------------
* 设置按钮
*-----------------------------------------------------------------------
  lw_action_line-id       = 'ADDROW_BASIC_01'.
  lw_action_line-visible  = cl_wd_uielement=>e_visible-visible.
*  lw_action_line-text     = text-001.
  lw_action_line-enabled  = abap_true.
  IF gv_from_preselection = 'X'.
    lw_action_line-enabled = abap_false.
  ENDIF.
  APPEND lw_action_line TO et_action_definition.
  lw_action_line-id       = 'DELROW_BASIC_02'.
  lw_action_line-visible  = cl_wd_uielement=>e_visible-visible.
*  lw_action_line-text     = text-002.
  lw_action_line-enabled  = abap_true.
  IF gv_from_preselection = 'X'.
    lw_action_line-enabled = abap_false.
  ENDIF.
  APPEND lw_action_line TO et_action_definition.
  lw_action_line-id       = 'GET_WLZMS'.
  lw_action_line-visible  = cl_wd_uielement=>e_visible-visible.
*  lw_action_line-text     = text-002.
  lw_action_line-enabled  = abap_true.
  APPEND lw_action_line TO et_action_definition.

ENDMETHOD.


METHOD if_fpm_guibb_list~process_event.

  DATA: lw_message        TYPE fpmgb_s_t100_message,
        lt_selected_lines TYPE rstabixtab,
        lw_selected_lines TYPE rstabix,
        lw_list           TYPE ty_list,
        lt_ztvenprd       TYPE STANDARD TABLE OF ztvenprd,
        lw_ztvenprd       TYPE ztvenprd.
  DATA: lt_ztventopur TYPE STANDARD TABLE OF ztventopur,
        lt_ztvenpur   TYPE STANDARD TABLE OF ztvenpur,
        lw_ztvenpur   TYPE ztvenpur.

  CASE io_event->mv_event_id.

**1 从其他步骤转入
*    WHEN cl_fpm_event=>gc_event_previous_step.
** 1.1 获取供应商编码
*      CALL METHOD mo_shared_data->if_fpm_parameter~get_value
*        EXPORTING
*          iv_key   = 'PARTNERNUM'
*        IMPORTING
*          ev_value = gv_partnernum.

*2 新增行
    WHEN 'ADDROW_BASIC_01'.
      CLEAR lw_list.
      lw_list-partnernum = gv_partnernum.
      APPEND lw_list TO gt_list.

*3 删除行
    WHEN 'DELROW_BASIC_02'.
* 3.1 校验没有选择数据则报错.
      IF it_selected_lines IS INITIAL.
        CLEAR:lw_message.
        lw_message-msgid = 'ZSRM'.
        lw_message-msgno = '008'.
        lw_message-severity = 'E'.
        APPEND lw_message TO et_messages.
        RETURN.
      ENDIF.
* 3.2 倒序排序，后一行一行删除
      lt_selected_lines = it_selected_lines.
      SORT lt_selected_lines DESCENDING BY tabix.
      LOOP AT it_selected_lines INTO lw_selected_lines.
        DELETE gt_list INDEX lw_selected_lines-tabix.
      ENDLOOP.

*4 取物料组描述
    WHEN 'GET_WLZMS'.
      READ TABLE gt_list INTO lw_list INDEX iv_event_index.
      IF sy-subrc = 0.
        SELECT SINGLE bottomclassd INTO lw_list-bottomclassd
          FROM ztventopur
         WHERE bottomclass = lw_list-bottomclass.
        MODIFY gt_list FROM lw_list INDEX iv_event_index TRANSPORTING bottomclassd.

        SELECT SINGLE topclass INTO lw_list-topclass
          FROM ztventopur
         WHERE bottomclass = lw_list-bottomclass.
        MODIFY gt_list FROM lw_list INDEX iv_event_index TRANSPORTING topclass.
      ENDIF.
*5 保存
    WHEN cl_fpm_event=>gc_event_save_draft.
* 5.1 必输字段校验
*      IF gt_list IS INITIAL.
*        CLEAR:lw_message.
*        lw_message-msgid = 'ZSRM'.
*        lw_message-msgno = '022'.
*        lw_message-severity = 'E'.
*        APPEND lw_message TO et_messages.
*        RETURN.
*      ENDIF.
*      LOOP AT gt_list INTO lw_list.
*        IF lw_list-topclass IS INITIAL.
*          CLEAR:lw_message.
*          lw_message-msgid = 'ZSRM'.
*          lw_message-msgno = '015'.
*          lw_message-severity = 'E'.
*          APPEND lw_message TO et_messages.
*          RETURN.
*        ENDIF.
*        IF lw_list-bottomclass IS INITIAL.
*          CLEAR:lw_message.
*          lw_message-msgid = 'ZSRM'.
*          lw_message-msgno = '016'.
*          lw_message-severity = 'E'.
*          APPEND lw_message TO et_messages.
*          RETURN.
*        ENDIF.
*      ENDLOOP.
* 5.2 保存数据,先删后保存
      LOOP AT gt_list INTO lw_list.
        CLEAR lw_ztvenprd.
        MOVE-CORRESPONDING lw_list TO lw_ztvenprd.
        APPEND lw_ztvenprd TO lt_ztvenprd.
      ENDLOOP.
      DELETE FROM ztvenprd WHERE partnernum = gv_partnernum.
      MODIFY ztvenprd FROM TABLE lt_ztvenprd.
      COMMIT WORK.

*6 转入下一步骤
    WHEN cl_fpm_event=>gc_event_next_step.
* 6.1 必输字段校验
      IF gt_list IS INITIAL.
        CLEAR:lw_message.
        lw_message-msgid = 'ZSRM'.
        lw_message-msgno = '022'.
        lw_message-severity = 'E'.
        APPEND lw_message TO et_messages.
        RETURN.
      ENDIF.
      LOOP AT gt_list INTO lw_list.
        IF lw_list-topclass IS INITIAL.
          CLEAR:lw_message.
          lw_message-msgid = 'ZSRM'.
          lw_message-msgno = '015'.
          lw_message-severity = 'E'.
          APPEND lw_message TO et_messages.
          RETURN.
        ENDIF.
        IF lw_list-bottomclass IS INITIAL.
          CLEAR:lw_message.
          lw_message-msgid = 'ZSRM'.
          lw_message-msgno = '016'.
          lw_message-severity = 'E'.
          APPEND lw_message TO et_messages.
          RETURN.
        ENDIF.
      ENDLOOP.
* 6.2 保存数据,先删后保存
      LOOP AT gt_list INTO lw_list.
        CLEAR lw_ztvenprd.
        MOVE-CORRESPONDING lw_list TO lw_ztvenprd.
        APPEND lw_ztvenprd TO lt_ztvenprd.
      ENDLOOP.
      DELETE FROM ztvenprd WHERE partnernum = gv_partnernum.
      MODIFY ztvenprd FROM TABLE lt_ztvenprd.

      IF gt_list IS NOT INITIAL.
        SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_ztventopur
          FROM ztventopur
           FOR ALL ENTRIES IN gt_list
         WHERE topclass = gt_list-topclass
           AND bottomclass = gt_list-bottomclass.
      ENDIF.
      LOOP AT gt_list INTO lw_list.
        MOVE-CORRESPONDING lw_list TO lw_ztvenpur.
        MOVE-CORRESPONDING lt_ztventopur[ topclass = lw_list-topclass bottomclass = lw_list-bottomclass ] TO lw_ztvenpur.
        APPEND lw_ztvenpur TO lt_ztvenpur.
      ENDLOOP.
      DELETE FROM ztvenpur WHERE partnernum = gv_partnernum.
      MODIFY ztvenpur FROM TABLE lt_ztvenpur.
      COMMIT WORK.

    WHEN OTHERS.
  ENDCASE.
ENDMETHOD.


  method IF_FPM_GUIBB~GET_PARAMETER_LIST.
  endmethod.


METHOD if_fpm_guibb~initialize.
  CALL METHOD y_cl_wd_tool=>get_value
    EXPORTING
      iv_key   = 'PARTNERNUM'
    IMPORTING
      ev_value = gv_partnernum.
  CALL METHOD y_cl_wd_tool=>get_value
    EXPORTING
      iv_key   = 'FROM_PRESELECTION'
    IMPORTING
      ev_value = gv_from_preselection.
ENDMETHOD.
ENDCLASS.
