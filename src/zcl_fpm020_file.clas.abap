CLASS zcl_fpm020_file DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_fpm_guibb .
    INTERFACES if_fpm_guibb_form .
    INTERFACES if_fpm_guibb_list .

    TYPES:
      BEGIN OF ty_form,
        alternum   TYPE ztvenalter-alternum,
        partner    TYPE but000-partner,
        name_org1  TYPE but000-name_org1,
        partnernum TYPE ztvenstatus-partnernum,
        applyuser  TYPE ztvenalter-applyuser,
      END OF ty_form .
    TYPES:
      BEGIN OF ty_list,
        alternum      TYPE ztvenfile3-alternum,
        certifctype   TYPE ztvenfile3-certifctype,
        certification TYPE ztvenfile3-certification,
        exzfnumb      TYPE ztvenfile3-exzfnumb,
        exmechanism   TYPE ztvenfile3-exmechanism,
        exissuedate   TYPE ztvenfile3-exissuedate,
        exstartdate   TYPE ztvenfile3-exstartdate,
        exdeadline    TYPE ztvenfile3-exdeadline,
        zfnumb        TYPE ztvenfile3-zfnumb,
        mechanism     TYPE ztvenfile3-mechanism,
        issuedate     TYPE ztvenfile3-issuedate,
        startdate     TYPE ztvenfile3-startdate,
        deadline      TYPE ztvenfile3-deadline,
        read_only_ref TYPE char1,
        exzfname      TYPE ztfile-zfname,
        zfname        TYPE ztfile-zfname,
        if_change     TYPE ztvenfile3-if_change,
        upload        TYPE char20,
        change_ref    TYPE char1,
        add_line_ref  TYPE char1,
        zfdesc        TYPE ztfile-zfdesc,
        zftype        TYPE ztfile-zftype,
        zfsize        TYPE ztfile-zfsize,
        zfsizm        TYPE ztfile-zfsizm,
        zfsizk        TYPE ztfile-zfsizk,
        zfpath        TYPE ztfile-zfpath,
        zfile         TYPE ztfile-zfile,
        zdatum        TYPE ztfile-zdatum,
        zuzeit        TYPE ztfile-zuzeit,
        zcrdby        TYPE ztfile-zcrdby,
        change_file   TYPE char1,
      END OF ty_list .
    TYPES:
      ty_t_list TYPE STANDARD TABLE OF ty_list .

    CLASS-DATA mo_shared_data TYPE REF TO y_cl_wd_shared_data .
    DATA gv_partner TYPE but000-partner .
    DATA gv_alternum TYPE ztvenalter-alternum .
    DATA gs_form TYPE ty_form .
    DATA gt_list TYPE ty_t_list .
    DATA gv_is_partner TYPE char1 .
    DATA gv_event_index TYPE i .

    METHODS m_handle_reject
      EXPORTING
        !et_messages TYPE fpmgb_t_messages .
    METHODS m_handle_accept
      EXPORTING
        !et_messages TYPE fpmgb_t_messages .
    METHODS m_download_file
      IMPORTING
        !iv_zfnumb    TYPE saeardoid OPTIONAL
        !iv_zfile     TYPE ztfile-zfile OPTIONAL
        !iv_filename  TYPE ztfile-zfname OPTIONAL
        !iv_mine_type TYPE ztfile-zftype OPTIONAL .
    METHODS m_handle_submit
      EXPORTING
        !et_messages TYPE fpmgb_t_messages .
protected section.
private section.
ENDCLASS.



CLASS ZCL_FPM020_FILE IMPLEMENTATION.


  method IF_FPM_GUIBB_FORM~CHECK_CONFIG.
  endmethod.


METHOD IF_FPM_GUIBB_FORM~FLUSH.
  FIELD-SYMBOLS: <fs_value> TYPE any.
  ASSIGN is_data->* TO <fs_value>.
  gs_form = <fs_value>.
ENDMETHOD.


METHOD if_fpm_guibb_form~get_data.
  DATA: lw_action_usage TYPE fpmgb_s_actionusage.

  CASE io_event->mv_event_id.
*1 初始化
    WHEN cl_fpm_event=>gc_event_open_dialog_box.
      CLEAR: gv_is_partner,gv_partner,gv_alternum,gs_form,gt_list.

*      实例化共享类
      mo_shared_data ?= y_cl_wd_shared_data=>get_instance( ).
      CLEAR: gv_is_partner,gv_partner,gv_alternum.
      mo_shared_data->if_fpm_parameter~get_value( EXPORTING iv_key = 'IS_PARTNER' IMPORTING ev_value = gv_is_partner ).
      mo_shared_data->if_fpm_parameter~get_value( EXPORTING iv_key = 'PARTNER' IMPORTING ev_value = gv_partner ).
      mo_shared_data->if_fpm_parameter~get_value( EXPORTING iv_key = 'ALTERNUM' IMPORTING ev_value = gv_alternum ).
      mo_shared_data->if_fpm_parameter~delete_value( iv_key = 'IS_PARTNER').
      mo_shared_data->if_fpm_parameter~delete_value( iv_key = 'PARTNER').
      mo_shared_data->if_fpm_parameter~delete_value( iv_key = 'ALTERNUM').

      gs_form-partner = gv_partner.
      gs_form-applyuser = sy-uname.
      IF gv_alternum IS INITIAL.
        gs_form-alternum = '#########'.
      ELSE.
        gs_form-alternum = gv_alternum.
      ENDIF.
      SELECT SINGLE name_org1 INTO gs_form-name_org1 FROM but000 WHERE partner = gv_partner.
      SELECT SINGLE partnernum INTO gs_form-partnernum FROM ztvenstatus WHERE formalcode = gv_partner.

      LOOP AT ct_action_usage INTO lw_action_usage.
        IF lw_action_usage-id = 'FILE_ACCEPT' OR lw_action_usage-id = 'FILE_REJECT'.
          IF gv_is_partner = 'X'.
            lw_action_usage-visible = '01'.
          ELSE.
            lw_action_usage-visible = '02'.
          ENDIF.
        ELSEIF lw_action_usage-id = 'FILE_SUBMIT'.
          IF gv_is_partner = 'X'.
            lw_action_usage-visible = '02'.
          ELSE.
            lw_action_usage-visible = '01'.
          ENDIF.
        ENDIF.
        IF lw_action_usage-id = 'FILE_ACCEPT'.
          SELECT COUNT(*) FROM ztvenalter WHERE alternum = gv_alternum AND ( status = '01' OR status = '03' ).
          IF sy-subrc <> 0.
            lw_action_usage-enabled = abap_false.
          ELSE.
            lw_action_usage-enabled = abap_true.
          ENDIF.
        ENDIF.
        IF lw_action_usage-id = 'FILE_REJECT'.
          SELECT COUNT(*) FROM ztvenalter WHERE alternum = gv_alternum AND status = '01'.
          IF sy-subrc <> 0.
            lw_action_usage-enabled = abap_false.
          ELSE.
            lw_action_usage-enabled = abap_true.
          ENDIF.
        ENDIF.

        IF lw_action_usage-id = 'FILE_SUBMIT'.
          SELECT COUNT(*) FROM ztvenalter WHERE alternum = gv_alternum AND status <> '01' AND status <> '03'.
          IF sy-subrc = 0.
            lw_action_usage-enabled = abap_false.
          ELSE.
            lw_action_usage-enabled = abap_true.
          ENDIF.
        ENDIF.
        MODIFY ct_action_usage FROM lw_action_usage.
      ENDLOOP.

      ev_action_usage_changed = abap_true.

    WHEN 'FILE_ACCEPT'.
      LOOP AT ct_action_usage INTO lw_action_usage.
        IF lw_action_usage-id = 'FILE_ACCEPT'.
          lw_action_usage-enabled = abap_false.
        ELSEIF lw_action_usage-id = 'FILE_REJECT'.
          lw_action_usage-enabled = abap_false.
        ENDIF.
        MODIFY ct_action_usage FROM lw_action_usage.
      ENDLOOP.
      ev_action_usage_changed = abap_true.

    WHEN 'FILE_REJECT'.
      LOOP AT ct_action_usage INTO lw_action_usage.
        IF lw_action_usage-id = 'FILE_ACCEPT'.
          lw_action_usage-enabled = abap_true.
        ELSEIF lw_action_usage-id = 'FILE_REJECT'.
          lw_action_usage-enabled = abap_false.
        ENDIF.
        MODIFY ct_action_usage FROM lw_action_usage.
      ENDLOOP.
      ev_action_usage_changed = abap_true.
    WHEN OTHERS.
  ENDCASE.

  cs_data = gs_form.
  ev_data_changed = abap_true.
ENDMETHOD.


  method IF_FPM_GUIBB_FORM~GET_DEFAULT_CONFIG.
  endmethod.


METHOD if_fpm_guibb_form~get_definition.
  DATA:lw_field_descr TYPE fpmgb_s_formfield_descr.
  DATA:lt_comp_tab    TYPE abap_component_tab.
  DATA:lw_comp_tab    TYPE abap_componentdescr.
  DATA:lw_action_line TYPE fpmgb_s_actiondef.

  eo_field_catalog ?=  cl_abap_structdescr=>describe_by_name( 'TY_FORM' ).
  lt_comp_tab = eo_field_catalog->get_components( ).

  LOOP AT lt_comp_tab INTO lw_comp_tab.
    lw_field_descr-name = lw_comp_tab-name.
    CASE lw_field_descr-name.
      WHEN 'ALTERNUM'.
        lw_field_descr-label_text = text-001.
      WHEN 'NAME_ORG1'.
        lw_field_descr-label_text = text-002.
      WHEN 'PARTNERNUM'.
        lw_field_descr-label_text = text-003.
      WHEN 'PARTNER'.
        lw_field_descr-label_text = text-004.
      WHEN 'APPLYUSER'.
        lw_field_descr-label_text = text-005.
      WHEN OTHERS.
    ENDCASE.

    APPEND lw_field_descr TO et_field_description.
    CLEAR lw_field_descr.
  ENDLOOP.

*-----------------------------------------------------------------------
* 设置按钮
*-----------------------------------------------------------------------
  CLEAR lw_action_line.
  lw_action_line-id       = 'FILE_SUBMIT'.
  lw_action_line-visible  = cl_wd_uielement=>e_visible-visible.
  lw_action_line-text     = text-006.
  lw_action_line-tooltip  = text-006.
  lw_action_line-enabled  = abap_true.
  APPEND lw_action_line TO et_action_definition.
  CLEAR lw_action_line.
  lw_action_line-id       = 'FILE_ACCEPT'.
  lw_action_line-visible  = cl_wd_uielement=>e_visible-visible.
  lw_action_line-text     = text-007.
  lw_action_line-tooltip  = text-007.
  lw_action_line-enabled  = abap_true.
  APPEND lw_action_line TO et_action_definition.
  CLEAR lw_action_line.
  lw_action_line-id       = 'FILE_REJECT'.
  lw_action_line-visible  = cl_wd_uielement=>e_visible-visible.
  lw_action_line-text     = text-008.
  lw_action_line-tooltip  = text-008.
  lw_action_line-enabled  = abap_true.
  APPEND lw_action_line TO et_action_definition.

ENDMETHOD.


METHOD IF_FPM_GUIBB_FORM~PROCESS_EVENT.
*  DATA: lt_ztvenchgct TYPE STANDARD TABLE OF ztvenchgct,
*        lw_message    TYPE fpmgb_s_t100_message,
*        lw_ztvenchgct TYPE ztvenchgct,
*        lw_ztvenalter TYPE ztvenalter.
*
*  CASE io_event->mv_event_id.
**1 提交
*    WHEN 'COTH_SUBMIT'.
** 1.1 删除原有的表数据
*      DELETE FROM ztvenchgct WHERE alternum = gv_alternum.
** 1.2 取修改编号
*      IF gv_alternum IS INITIAL.
*        zcl_fpm020_spinfo_change=>m_get_alternum( IMPORTING ev_alternum = gv_alternum ).
*      ENDIF.
*
*      MOVE-CORRESPONDING gt_list TO lt_ztvenchgct.
*      LOOP AT lt_ztvenchgct INTO lw_ztvenchgct.
*        lw_ztvenchgct-alternum = gv_alternum.
*        MODIFY lt_ztvenchgct FROM lw_ztvenchgct.
*      ENDLOOP.
*
*      lw_ztvenalter-alternum = gv_alternum.
*      lw_ztvenalter-altertype = '03'.
*      lw_ztvenalter-applydate = sy-datum.
*      lw_ztvenalter-applytime = sy-uzeit.
*      lw_ztvenalter-applyuser = sy-uname.
*      lw_ztvenalter-status = '01'.
*      lw_ztvenalter-partnernum = gs_form-partnernum.
*      lw_ztvenalter-formalcode = gv_partner.
** 1.3 更新表数据
*      MODIFY ztvenalter FROM lw_ztvenalter.
*      MODIFY ztvenchgct FROM TABLE lt_ztvenchgct.
*      IF sy-subrc = 0.
*        CLEAR:lw_message.
*        lw_message-msgid = 'ZSRM'.
*        lw_message-msgno = '041'.
*        lw_message-severity = 'S'.
*        APPEND lw_message TO et_messages.
*      ENDIF.
*
**2 接受并更新
*    WHEN 'COTH_ACCEPT'.
*      CALL METHOD me->m_handle_accept
*        IMPORTING
*          et_messages = et_messages.
*
**3 拒绝
*    WHEN 'COTH_REJECT'.
*      CALL METHOD me->m_handle_reject
*        IMPORTING
*          et_messages = et_messages.
*
*
*    WHEN OTHERS.
*  ENDCASE.
ENDMETHOD.


  method IF_FPM_GUIBB_LIST~CHECK_CONFIG.
  endmethod.


METHOD IF_FPM_GUIBB_LIST~FLUSH.
  FIELD-SYMBOLS: <fs_value> TYPE any.
  ASSIGN it_data->* TO <fs_value>.
  gt_list = <fs_value>.

ENDMETHOD.


METHOD if_fpm_guibb_list~get_data.
  DATA: lw_field_usage  TYPE fpmgb_s_fieldusage,
        lw_action_usage TYPE fpmgb_s_actionusage,
        lw_list         TYPE ty_list,
        lt_ztfile       TYPE STANDARD TABLE OF ztfile,
        lw_ztfile       TYPE ztfile.

  CASE iv_eventid->mv_event_id.
*1 初始化
    WHEN cl_fpm_event=>gc_event_open_dialog_box.
*1.1 取数初始化
      IF gv_alternum IS NOT INITIAL.
        SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_list
          FROM ztvenfile3
         WHERE alternum = gv_alternum.
        LOOP AT gt_list INTO lw_list.
          IF lw_list-if_change = 'X'.
            lw_list-change_ref = abap_false.
          ELSE.
            lw_list-change_ref = abap_true.
          ENDIF.

          lw_list-upload = text-024.
          lw_list-add_line_ref = abap_true.
          MODIFY gt_list FROM lw_list.
        ENDLOOP.


      ELSE.
        SELECT
          certification
          certifctype
          zfnumb AS exzfnumb
          mechanism AS exmechanism
          issuedate AS exissuedate
          startdate AS exstartdate
          deadline AS exdeadline
          INTO CORRESPONDING FIELDS OF TABLE gt_list
          FROM ztvenfile2
         WHERE partnernum = gs_form-partnernum
           AND own = 'X'.

        LOOP AT gt_list INTO lw_list.
          lw_list-upload = text-024.
          lw_list-if_change = ''.
          lw_list-change_ref = abap_true.
          lw_list-add_line_ref = abap_true.
          MODIFY gt_list FROM lw_list.
        ENDLOOP.

      ENDIF.

      SELECT * APPENDING CORRESPONDING FIELDS OF TABLE lt_ztfile
        FROM ztfile
         FOR ALL ENTRIES IN gt_list
       WHERE zfnumb = gt_list-zfnumb.
      SELECT * APPENDING CORRESPONDING FIELDS OF TABLE lt_ztfile
        FROM ztfile
         FOR ALL ENTRIES IN gt_list
       WHERE zfnumb = gt_list-exzfnumb.
      SORT lt_ztfile BY zfnumb.
      LOOP AT gt_list INTO lw_list .
        TRY .
            lw_list-zfname = lt_ztfile[ zfnumb = lw_list-zfnumb ]-zfname.
            lw_list-exzfname = lt_ztfile[ zfnumb = lw_list-exzfnumb ]-zfname.
          CATCH  cx_sy_itab_line_not_found .
        ENDTRY.
        MODIFY gt_list FROM lw_list.
      ENDLOOP.

      LOOP AT gt_list INTO lw_list.
        lw_list-upload = text-024.
        MODIFY gt_list FROM lw_list.
      ENDLOOP.

* 1.2 采购员账号不可编辑
      LOOP AT ct_field_usage INTO lw_field_usage.
        IF gv_is_partner IS INITIAL.
          IF lw_field_usage-name = 'UPLOAD'.
            lw_field_usage-enabled = abap_false.
          ELSEIF lw_field_usage-name = 'IF_CHANGE'.
            lw_field_usage-read_only = abap_true.
          ENDIF.
        ENDIF.
        MODIFY ct_field_usage FROM lw_field_usage.
      ENDLOOP.
      LOOP AT ct_action_usage INTO lw_action_usage.
        IF gv_is_partner IS INITIAL.
          lw_action_usage-visible = '01'.
        ENDIF.
        MODIFY ct_action_usage FROM lw_action_usage.
      ENDLOOP.
      IF gv_is_partner = ''.
        LOOP AT gt_list INTO lw_list.
          lw_list-change_ref = abap_true.
          MODIFY gt_list FROM lw_list.
        ENDLOOP.
      ENDIF.

      ev_field_usage_changed = abap_true.
      ev_action_usage_changed = abap_true.

*2 选择是否变更
    WHEN 'FILE_IF_CHANGE'.
      READ TABLE gt_list INTO lw_list INDEX cv_lead_index.
      IF sy-subrc = 0.
        IF lw_list-if_change = 'X'.
          lw_list-change_ref = abap_false.
        ELSE.
          lw_list-change_ref = abap_true.
        ENDIF.
        MODIFY gt_list FROM lw_list INDEX cv_lead_index.
      ENDIF.
*      LOOP AT ct_action_usage INTO lw_action_usage.
*        IF lw_action_usage-id = 'FILE_UPLOAD_FILE' OR
*           lw_action_usage-id = 'FILE_DOWNLOAD_FILE'.
*          IF lw_list-if_change = 'X'.
*            lw_action_usage-enabled = abap_true.
*            lw_action_usage-visible = '02'.
*          ELSE.
*            lw_action_usage-enabled = abap_false.
*            lw_action_usage-visible = '01'.
*          ENDIF.
*        ENDIF.
*        MODIFY ct_action_usage FROM lw_action_usage.
*      ENDLOOP.
      ev_field_usage_changed = abap_true.
*      ev_action_usage_changed = abap_true.

*3 关闭上传文件对话框
    WHEN cl_fpm_event=>gc_event_close_dialog_box.
      CLEAR lw_ztfile.
      mo_shared_data->get_value( EXPORTING iv_key   = 'ZFILE' IMPORTING ev_value = lw_ztfile ).
      mo_shared_data->if_fpm_parameter~delete_value( EXPORTING iv_key = 'ZFILE' ).

      IF lw_ztfile IS NOT INITIAL.
        READ TABLE gt_list INTO lw_list INDEX gv_event_index.
        lw_ztfile-zfnumb = lw_list-zfnumb.
        MOVE-CORRESPONDING lw_ztfile TO lw_list.
        lw_list-change_file = 'X'.
        MODIFY gt_list FROM lw_list INDEX gv_event_index.
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
    CASE lw_field_descr-name.
      WHEN 'CERTIFCTYPE'.
        lw_field_descr-read_only_ref = 'ADD_LINE_REF'.
      WHEN 'CERTIFICATION'.
        lw_field_descr-header_label = text-009.
        lw_field_descr-read_only_ref = 'ADD_LINE_REF'.
      WHEN 'EXZFNAME'.
        lw_field_descr-header_label = text-010.
      WHEN 'EXMECHANISM'.
        lw_field_descr-header_label = text-011.
      WHEN 'EXISSUEDATE'.
        lw_field_descr-header_label = text-012.
      WHEN 'EXSTARTDATE'.
        lw_field_descr-header_label = text-013.
      WHEN 'EXDEADLINE'.
        lw_field_descr-header_label = text-014.
      WHEN 'IF_CHANGE'.
        lw_field_descr-header_label = text-015.
      WHEN 'UPLOAD'.
        lw_field_descr-header_label = text-016.
        lw_field_descr-read_only_ref = 'CHANGE_REF'.
        lw_field_descr-enabled_ref = 'IF_CHANGE'.
      WHEN 'ZFNAME'.
        lw_field_descr-header_label = text-017.
        lw_field_descr-read_only_ref = 'CHANGE_REF'.
        lw_field_descr-mandatory = abap_true.
      WHEN 'MECHANISM'.
        lw_field_descr-header_label = text-018.
        lw_field_descr-read_only_ref = 'CHANGE_REF'.
        lw_field_descr-mandatory = abap_true.
      WHEN 'ISSUEDATE'.
        lw_field_descr-header_label = text-019.
        lw_field_descr-read_only_ref = 'CHANGE_REF'.
        lw_field_descr-mandatory = abap_true.
      WHEN 'STARTDATE'.
        lw_field_descr-header_label = text-020.
        lw_field_descr-read_only_ref = 'CHANGE_REF'.
        lw_field_descr-mandatory = abap_true.
      WHEN 'DEADLINE'.
        lw_field_descr-header_label = text-021.
        lw_field_descr-read_only_ref = 'CHANGE_REF'.
        lw_field_descr-mandatory = abap_true.
      WHEN 'CERTIFCTYPE'.
        lw_field_descr-read_only = abap_true.
      WHEN OTHERS.
    ENDCASE.

    APPEND lw_field_descr TO et_field_description.
    CLEAR lw_field_descr.
  ENDLOOP.

*-----------------------------------------------------------------------
* 设置按钮
*-----------------------------------------------------------------------
  CLEAR lw_action_line.
  lw_action_line-id       = 'FILE_ADD_ROW'.
  lw_action_line-visible  = cl_wd_uielement=>e_visible-visible.
  lw_action_line-enabled  = abap_true.
  lw_action_line-action_type  = 1.
  APPEND lw_action_line TO et_action_definition.
  CLEAR lw_action_line.
  lw_action_line-id       = 'FILE_DEL_ROW'.
  lw_action_line-visible  = cl_wd_uielement=>e_visible-visible.
  lw_action_line-enabled  = abap_true.
  lw_action_line-action_type  = 1.
  APPEND lw_action_line TO et_action_definition.
  CLEAR lw_action_line.
  lw_action_line-id       = 'FILE_IF_CHANGE'.
  lw_action_line-visible  = cl_wd_uielement=>e_visible-visible.
  lw_action_line-enabled  = abap_true.
  lw_action_line-action_type  = 1.
  APPEND lw_action_line TO et_action_definition.
  CLEAR lw_action_line.
  lw_action_line-id       = 'FILE_DOWNLOAD_EXFILE'.
  lw_action_line-visible  = cl_wd_uielement=>e_visible-visible.
  lw_action_line-enabled  = abap_true.
  lw_action_line-action_type  = 1.
  APPEND lw_action_line TO et_action_definition.
  CLEAR lw_action_line.
  lw_action_line-id       = 'FILE_DOWNLOAD_FILE'.
  lw_action_line-visible  = cl_wd_uielement=>e_visible-visible.
  lw_action_line-enabled  = abap_true.
  lw_action_line-action_type  = 1.
  APPEND lw_action_line TO et_action_definition.
  CLEAR lw_action_line.
  lw_action_line-id       = 'FILE_UPLOAD_FILE'.
  lw_action_line-visible  = cl_wd_uielement=>e_visible-visible.
  lw_action_line-enabled  = abap_true.
  lw_action_line-action_type  = 1.
  APPEND lw_action_line TO et_action_definition.

ENDMETHOD.


METHOD if_fpm_guibb_list~process_event.
  DATA: lw_list           TYPE ty_list,
        lw_message        TYPE fpmgb_s_t100_message,
        lt_selected_lines TYPE rstabixtab,
        lw_selected_lines TYPE rstabix,
        lt_list           TYPE STANDARD TABLE OF ty_list,
        lv_counter        TYPE i.
  DATA: lw_param TYPE fpm_s_dialog_box_properties.


  CASE io_event->mv_event_id.
*1 新增行
    WHEN 'FILE_ADD_ROW'.
      lw_list-alternum = gv_alternum.
      lw_list-upload = text-024.
      lw_list-if_change = 'X'.
      lw_list-change_ref = abap_false.
      APPEND lw_list TO gt_list.

*2 删除行
    WHEN 'FILE_DEL_ROW'.
* 2.1 校验没有选择数据则报错.
      IF it_selected_lines IS INITIAL.
        CLEAR:lw_message.
        lw_message-msgid = 'ZSRM'.
        lw_message-msgno = '008'.
        lw_message-severity = 'E'.
        APPEND lw_message TO et_messages.
        RETURN.
      ENDIF.
* 2.2 倒序排序，后一行一行删除
      IF gt_list[ iv_lead_index ]-exzfname IS NOT INITIAL.
        CLEAR:lw_message.
        lw_message-msgid = 'ZSRM'.
        lw_message-msgno = '026'.
        lw_message-severity = 'E'.
        APPEND lw_message TO et_messages.
        RETURN.
      ELSE.
        DELETE gt_list INDEX iv_lead_index.
      ENDIF.

*3 下载原附件
    WHEN 'FILE_DOWNLOAD_EXFILE'.
      me->m_download_file( EXPORTING iv_zfnumb = gt_list[ iv_event_index ]-exzfnumb ).

*4 下载新附件
    WHEN 'FILE_DOWNLOAD_FILE'.
      me->m_download_file( EXPORTING iv_zfnumb    = gt_list[ iv_event_index ]-zfnumb
                                     iv_zfile     = gt_list[ iv_event_index ]-zfile
                                     iv_filename  = gt_list[ iv_event_index ]-zfname
                                     iv_mine_type = gt_list[ iv_event_index ]-zftype ).

*5 上传附件
    WHEN 'FILE_UPLOAD_FILE'.
      lw_param-height = '200'.
      lw_param-width = '600'.
      lw_param-title = text-025.
      y_cl_wd_tool=>open_dialog( iv_param = lw_param iv_page = 'UPLOAD_FILE' ).
      gv_event_index = iv_event_index.

*6 提交
    WHEN 'FILE_SUBMIT'.
      me->m_handle_submit( IMPORTING et_messages = et_messages ).

*6 接受并更新
    WHEN 'FILE_ACCEPT'.
      me->m_handle_accept( IMPORTING et_messages = et_messages ).

*6 提交
    WHEN 'FILE_REJECT'.
      me->m_handle_reject( IMPORTING et_messages = et_messages ).


    WHEN OTHERS.
  ENDCASE.
ENDMETHOD.


  method IF_FPM_GUIBB~GET_PARAMETER_LIST.
  endmethod.


METHOD IF_FPM_GUIBB~INITIALIZE.


ENDMETHOD.


METHOD m_download_file.
  DATA: lv_file      TYPE xstring,
        lv_filename  TYPE string,
        lv_mine_type TYPE string.

  CHECK iv_zfnumb IS NOT INITIAL OR
        ( iv_zfile IS NOT INITIAL AND
          iv_filename IS NOT INITIAL AND
          iv_mine_type IS NOT INITIAL ).

  IF iv_zfnumb IS NOT INITIAL.
    CALL FUNCTION 'ZSCMS_READ_DOWNLOAD'
      EXPORTING
        sap_object = 'ZSRM'
        arc_doc_id = iv_zfnumb
      IMPORTING
        file       = lv_file
        filename   = lv_filename
        mime_type  = lv_mine_type.

  ELSE.
    lv_file = iv_zfile.
    lv_filename = iv_filename.
    lv_mine_type = iv_mine_type.
  ENDIF.
* 2.2 执行下载
  CALL METHOD cl_wd_runtime_services=>attach_file_to_response
    EXPORTING
      i_filename  = lv_filename
      i_content   = lv_file
      i_mime_type = lv_mine_type.
ENDMETHOD.


METHOD m_handle_accept.
  DATA: lw_message    TYPE fpmgb_s_t100_message,
        lt_list       TYPE STANDARD TABLE OF ty_list,
        lt_ztvenfile2 TYPE STANDARD TABLE OF ztvenfile2,
        lw_ztvenfile2 TYPE ztvenfile2,
        lw_ztvenalter TYPE ztvenalter,
        lw_list       TYPE ty_list.

**1 数据校验
*  SELECT COUNT(*) FROM ztvenalter WHERE alternum = gv_alternum AND status = '01'.
*  IF sy-subrc <> 0.
*    CLEAR:lw_message.
*    lw_message-msgid = 'ZSRM'.
*    lw_message-msgno = '113'.
*    lw_message-severity = 'E'.
*    APPEND lw_message TO et_messages.
*    RETURN.
*  ENDIF.



*3 更新数据库表
  lt_list = gt_list.
  DELETE lt_list WHERE if_change = ''.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_ztvenfile2
    FROM ztvenfile2
     FOR ALL ENTRIES IN lt_list
   WHERE partnernum = gs_form-partnernum
     AND certification = lt_list-certification.
  SORT lt_ztvenfile2 BY certification.
  LOOP AT lt_list INTO lw_list.
    CLEAR lw_ztvenfile2.
    READ TABLE lt_ztvenfile2 INTO lw_ztvenfile2
      WITH KEY certification = lw_list-certification BINARY SEARCH.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING lw_list TO lw_ztvenfile2.
      MODIFY lt_ztvenfile2 FROM lw_ztvenfile2 INDEX sy-tabix.
    ELSE.
      MOVE-CORRESPONDING lw_list TO lw_ztvenfile2.
      lw_ztvenfile2-partnernum = gs_form-partnernum.
      IF lw_list-zfname IS INITIAL.
        lw_ztvenfile2-own = ''.
      ELSE.
        lw_ztvenfile2-own = 'X'.
      ENDIF.
      APPEND lw_ztvenfile2 TO lt_ztvenfile2.
    ENDIF.
  ENDLOOP.
  MODIFY ztvenfile2 FROM TABLE lt_ztvenfile2.

  SELECT SINGLE * INTO CORRESPONDING FIELDS OF lw_ztvenalter
    FROM ztvenalter
   WHERE alternum = gv_alternum.
  lw_ztvenalter-status = '07'.
  lw_ztvenalter-puruser = sy-uname.
  lw_ztvenalter-purdate = sy-datum.
  lw_ztvenalter-updateuser = sy-uname.
  lw_ztvenalter-updatedate = sy-datum.
  MODIFY ztvenalter FROM lw_ztvenalter.

*4 输出消息
  CLEAR:lw_message.
  lw_message-msgid = 'ZSRM'.
  lw_message-msgno = '041'.
  lw_message-severity = 'S'.
  APPEND lw_message TO et_messages.
ENDMETHOD.


METHOD m_handle_reject.
  DATA: lw_message    TYPE fpmgb_s_t100_message,
        lt_ztvencoth  TYPE STANDARD TABLE OF ztvencoth,
        lw_ztvenalter TYPE ztvenalter.

*1 更新数据库表
  SELECT SINGLE * INTO CORRESPONDING FIELDS OF lw_ztvenalter
    FROM ztvenalter
   WHERE alternum = gv_alternum.
  lw_ztvenalter-status = '03'.
  lw_ztvenalter-puruser = sy-uname.
  lw_ztvenalter-purdate = sy-datum.
  MODIFY ztvenalter FROM lw_ztvenalter.


*2 输出消息
  CLEAR:lw_message.
  lw_message-msgid = 'ZSRM'.
  lw_message-msgno = '041'.
  lw_message-severity = 'S'.
  APPEND lw_message TO et_messages.


ENDMETHOD.


METHOD m_handle_submit.
  DATA: lw_message    TYPE fpmgb_s_t100_message,
        lt_ztvencoth  TYPE STANDARD TABLE OF ztvencoth,
        lw_ztvenalter TYPE ztvenalter,
        lt_list       TYPE STANDARD TABLE OF ty_list,
        lw_list       TYPE ty_list,
        lv_counter    TYPE i,
        lw_ztfile     TYPE ztfile,
        lt_ztfile     TYPE STANDARD TABLE OF ztfile,
        lt_ztvenfile3 TYPE STANDARD TABLE OF ztvenfile3,
        lt_ztvenfile2 TYPE STANDARD TABLE OF ztvenfile2,
        lw_ztvenfile2 TYPE ztvenfile2.
  DATA: lv_objid       TYPE sapb-sapobjid,
        lv_xstring     TYPE xstring,
        lv_filename255 TYPE toaat-filename.

*1 数据校验
* 1.1 未选择变更
  READ TABLE gt_list TRANSPORTING NO FIELDS
    WITH KEY if_change = 'X'.
  IF sy-subrc <> 0.
    CLEAR:lw_message.
    lw_message-msgid = 'ZSRM'.
    lw_message-msgno = '114'.
    lw_message-severity = 'E'.
    APPEND lw_message TO et_messages.
    RETURN.
  ENDIF.
* 1.2 必输校验
  LOOP AT gt_list INTO lw_list WHERE if_change = 'X'.
    IF lw_list-mechanism IS INITIAL OR
        lw_list-issuedate IS INITIAL OR
        lw_list-startdate IS INITIAL OR
        lw_list-deadline IS INITIAL OR
        lw_list-zfname IS INITIAL.
      CLEAR:lw_message.
      lw_message-msgid = 'ZSRM'.
      lw_message-msgno = '009'.
      lw_message-severity = 'E'.
      APPEND lw_message TO et_messages.
      RETURN.
    ENDIF.
  ENDLOOP.
* 1.3 重复校验
  LOOP AT gt_list INTO lw_list.
    lt_list = gt_list.
    DELETE lt_list WHERE certifctype <> lw_list-certifctype OR certification <> lw_list-certification.
    DESCRIBE TABLE lt_list LINES lv_counter.
    IF lv_counter > 1.
      CLEAR:lw_message.
      lw_message-msgid = 'ZSRM'.
      lw_message-msgno = '115'.
      lw_message-severity = 'E'.
      lw_message-parameter_1 = lw_list-certification.
      APPEND lw_message TO et_messages.
      RETURN.
    ENDIF.
  ENDLOOP.
* 1.4 有效截止日期检查
  LOOP AT gt_list INTO lw_list WHERE if_change = 'X'.
    IF lw_list-deadline < sy-datum OR lw_list-deadline < lw_list-startdate.
      CLEAR:lw_message.
      lw_message-msgid = 'ZSRM'.
      lw_message-msgno = '145'.
      lw_message-severity = 'E'.
      APPEND lw_message TO et_messages.
      RETURN.
    ENDIF.
  ENDLOOP.


*2 更新数据
* 2.1 取变更编号
  IF gv_alternum IS INITIAL.
    zcl_fpm020_spinfo_change=>m_get_alternum( IMPORTING ev_alternum = gv_alternum ).
    gs_form-alternum = gv_alternum.
    LOOP AT gt_list INTO lw_list.
      lw_list-alternum = gv_alternum.
      MODIFY gt_list FROM lw_list.
    ENDLOOP.
  ENDIF.
* 2.2 写入数据库表
*  2.2.1 附件上传服务器
  LOOP AT gt_list INTO lw_list WHERE change_file = 'X'.
*   2.2.1.1 删除旧附件
    IF lw_list-zfnumb IS NOT INITIAL.
      lv_objid = gs_form-partnernum.
      CALL FUNCTION 'ZSCMS_DELETE'
        EXPORTING
          mandt                     = sy-mandt
          archiv_doc_id             = lw_list-zfnumb
          archiv_id                 = 'Z1'
          sap_object                = 'ZSRM'
          object_id                 = lv_objid
        EXCEPTIONS
          delete_by_key_error       = 1
          archivobject_delete_error = 2
          OTHERS                    = 3.
    ENDIF.
    lv_xstring = lw_list-zfile.
    lv_filename255 = lw_list-zfname.
    lv_objid = gs_form-partnernum.
    CALL FUNCTION 'ZSCMS_XSTRING_UPLOAD'
      EXPORTING
        l_xstring            = lv_xstring
        filename             = lv_filename255
        ar_object            = 'ZFILETYPE'
        object_id            = lv_objid
        sap_object           = 'ZSRM'
      IMPORTING
        arc_doc_id           = lw_list-zfnumb
      EXCEPTIONS
        mimetype_not_find    = 1
        xstring_create_error = 2
        file_insert_error    = 3
        OTHERS               = 4.
    CLEAR lw_list-change_file.
    MODIFY gt_list FROM lw_list.
  ENDLOOP.
*  2.2.2 更新ZTFILE
  LOOP AT gt_list INTO lw_list.
    CLEAR lw_ztfile.
    MOVE-CORRESPONDING lw_list TO lw_ztfile.
    CLEAR lw_ztfile-zfile.
    APPEND lw_ztfile TO lt_ztfile.
  ENDLOOP.
  MODIFY ztfile FROM TABLE lt_ztfile.
*  2.2.3 更新ZTVENFILE3
  MOVE-CORRESPONDING gt_list TO lt_ztvenfile3.
  DELETE FROM ztvenfile3 WHERE alternum = gs_form-alternum.
  MODIFY ztvenfile3 FROM TABLE lt_ztvenfile3.
**  2.2.4 更新ZTFVENFILE2
*  LOOP AT gt_list INTO lw_list WHERE if_change = 'X'.
*    MOVE-CORRESPONDING lw_list TO lw_ztvenfile2.
*    lw_ztvenfile2-partnernum = gs_form-partnernum.
*    IF lw_list-zfnumb IS NOT INITIAL.
*      lw_ztvenfile2-own = 'X'.
*    ENDIF.
*    APPEND lw_ztvenfile2 TO lt_ztvenfile2.
*  ENDLOOP.
*  MODIFY ztvenfile2 FROM TABLE lt_ztvenfile2.
*  2.2.5 更新ZTVENALTER
  CLEAR lw_ztvenalter.
  SELECT SINGLE * INTO CORRESPONDING FIELDS OF lw_ztvenalter
    FROM ztvenalter
   WHERE alternum = gs_form-alternum.
  IF sy-subrc = 0.
    lw_ztvenalter-applydate = sy-datum.
    lw_ztvenalter-applytime = sy-uzeit.
    lw_ztvenalter-applyuser = sy-uname.
  ELSE.
    lw_ztvenalter-alternum = gs_form-alternum.
    lw_ztvenalter-altertype = '02'.
    lw_ztvenalter-applydate = sy-datum.
    lw_ztvenalter-applytime = sy-uzeit.
    lw_ztvenalter-applyuser = sy-uname.
    lw_ztvenalter-status = '01'.
    lw_ztvenalter-partnernum = gs_form-partnernum.
    lw_ztvenalter-formalcode = gs_form-partner.
    lw_ztvenalter-applyuser = sy-uname.
  ENDIF.
  MODIFY ztvenalter FROM lw_ztvenalter.


*3 输出消息
  CLEAR:lw_message.
  lw_message-msgid = 'ZSRM'.
  lw_message-msgno = '041'.
  lw_message-severity = 'S'.
  APPEND lw_message TO et_messages.


ENDMETHOD.
ENDCLASS.
