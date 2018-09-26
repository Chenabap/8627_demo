class ZCL_FPM020_COTH definition
  public
  final
  create public .

public section.

  interfaces IF_FPM_GUIBB .
  interfaces IF_FPM_GUIBB_FORM .
  interfaces IF_FPM_GUIBB_LIST .

  types:
    BEGIN OF ty_form,
        partner    TYPE but000-partner,
        name_org1  TYPE but000-name_org1,
        partnernum TYPE ztvenstatus-partnernum,
      END OF ty_form .
  types:
    BEGIN OF ty_list,
        alternum      TYPE ztvenchgct-alternum,
        partnernum    TYPE ztvenchgct-partnernum,
        zposition     TYPE ztvenchgct-zposition,
        name          TYPE ztvenchgct-name,
        phonenum      TYPE ztvenchgct-phonenum,
        mobilephone   TYPE ztvenchgct-mobilephone,
        emailadd      TYPE ztvenchgct-emailadd,
        faxnum        TYPE ztvenchgct-faxnum,
        notes         TYPE ztvenchgct-notes,
        read_only_ref TYPE char1,
      END OF ty_list .
  types:
    ty_t_list TYPE STANDARD TABLE OF ty_list .

  class-data MO_SHARED_DATA type ref to Y_CL_WD_SHARED_DATA .
  data GV_PARTNER type BUT000-PARTNER .
  data GV_ALTERNUM type ZTVENALTER-ALTERNUM .
  data GS_FORM type TY_FORM .
  data GT_LIST type TY_T_LIST .
  data GV_IS_PARTNER type CHAR1 .

  methods M_HANDLE_REJECT
    exporting
      !ET_MESSAGES type FPMGB_T_MESSAGES .
  methods M_HANDLE_ACCEPT
    exporting
      !ET_MESSAGES type FPMGB_T_MESSAGES .
protected section.
private section.
ENDCLASS.



CLASS ZCL_FPM020_COTH IMPLEMENTATION.


  method IF_FPM_GUIBB_FORM~CHECK_CONFIG.
  endmethod.


METHOD if_fpm_guibb_form~flush.
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
      mo_shared_data->if_fpm_parameter~get_value( EXPORTING iv_key = 'IS_PARTNER' IMPORTING ev_value = gv_is_partner ).
      mo_shared_data->if_fpm_parameter~get_value( EXPORTING iv_key = 'PARTNER' IMPORTING ev_value = gv_partner ).
      mo_shared_data->if_fpm_parameter~get_value( EXPORTING iv_key = 'ALTERNUM' IMPORTING ev_value = gv_alternum ).
      mo_shared_data->if_fpm_parameter~delete_value( iv_key = 'IS_PARTNER').
      mo_shared_data->if_fpm_parameter~delete_value( iv_key = 'PARTNER').
      mo_shared_data->if_fpm_parameter~delete_value( iv_key = 'ALTERNUM').

      gs_form-partner = gv_partner.
      SELECT SINGLE name_org1 INTO gs_form-name_org1 FROM but000 WHERE partner = gv_partner.
      SELECT SINGLE partnernum INTO gs_form-partnernum FROM ztvenstatus WHERE formalcode = gv_partner.

      LOOP AT ct_action_usage INTO lw_action_usage.
        IF lw_action_usage-id = 'COTH_ACCEPT' OR lw_action_usage-id = 'COTH_REJECT'.
          IF gv_is_partner = 'X'.
            lw_action_usage-visible = '01'.
          ELSE.
            lw_action_usage-visible = '02'.
          ENDIF.
        ENDIF.
        IF lw_action_usage-id = 'COTH_ACCEPT'.
          SELECT COUNT(*) FROM ztvenalter WHERE alternum = gv_alternum AND ( status = '01' OR status = '08' OR status = '03' ).
          IF sy-subrc <> 0.
            lw_action_usage-enabled = abap_false.
          ELSE.
            lw_action_usage-enabled = abap_true.
          ENDIF.
        ENDIF.
        IF lw_action_usage-id = 'COTH_REJECT'.
          SELECT COUNT(*) FROM ztvenalter WHERE alternum = gv_alternum AND status = '01'.
          IF sy-subrc <> 0.
            lw_action_usage-enabled = abap_false.
          ELSE.
            lw_action_usage-enabled = abap_true.
          ENDIF.
        ENDIF.

        IF lw_action_usage-id = 'COTH_SUBMIT'.
          IF gv_is_partner = 'X'.
            lw_action_usage-visible = '02'.
          ELSE.
            lw_action_usage-visible = '01'.
          ENDIF.

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

    WHEN 'COTH_ACCEPT'.
      LOOP AT ct_action_usage INTO lw_action_usage.
        IF lw_action_usage-id = 'COTH_ACCEPT'.
          lw_action_usage-enabled = abap_false.
        ELSEIF lw_action_usage-id = 'COTH_REJECT'.
          lw_action_usage-enabled = abap_false.
        ENDIF.
        MODIFY ct_action_usage FROM lw_action_usage.
      ENDLOOP.
      ev_action_usage_changed = abap_true.

    WHEN 'COTH_REJECT'.
      LOOP AT ct_action_usage INTO lw_action_usage.
        IF lw_action_usage-id = 'COTH_ACCEPT'.
          lw_action_usage-enabled = abap_true.
        ELSEIF lw_action_usage-id = 'COTH_REJECT'.
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
      WHEN 'PARTNER'.
        lw_field_descr-label_text = text-001.
      WHEN 'NAME_ORG1'.
        lw_field_descr-label_text = text-002.
      WHEN 'PARTNERNUM'.
        lw_field_descr-label_text = text-003.
      WHEN OTHERS.
    ENDCASE.

    lw_field_descr-read_only = abap_true.

    APPEND lw_field_descr TO et_field_description.
    CLEAR lw_field_descr.
  ENDLOOP.

*-----------------------------------------------------------------------
* 设置按钮
*-----------------------------------------------------------------------
  CLEAR lw_action_line.
  lw_action_line-id       = 'COTH_SUBMIT'.
  lw_action_line-visible  = cl_wd_uielement=>e_visible-visible.
  lw_action_line-text     = text-004.
  lw_action_line-tooltip  = text-004.
  lw_action_line-enabled  = abap_true.
  APPEND lw_action_line TO et_action_definition.
  CLEAR lw_action_line.
  lw_action_line-id       = 'COTH_ACCEPT'.
  lw_action_line-visible  = cl_wd_uielement=>e_visible-visible.
  lw_action_line-text     = text-005.
  lw_action_line-tooltip  = text-005.
  lw_action_line-enabled  = abap_true.
  APPEND lw_action_line TO et_action_definition.
  CLEAR lw_action_line.
  lw_action_line-id       = 'COTH_REJECT'.
  lw_action_line-visible  = cl_wd_uielement=>e_visible-visible.
  lw_action_line-text     = text-006.
  lw_action_line-tooltip  = text-006.
  lw_action_line-enabled  = abap_true.
  APPEND lw_action_line TO et_action_definition.

ENDMETHOD.


METHOD if_fpm_guibb_form~process_event.
  DATA: lt_ztvenchgct TYPE STANDARD TABLE OF ztvenchgct,
        lw_message    TYPE fpmgb_s_t100_message,
        lw_ztvenchgct TYPE ztvenchgct,
        lw_ztvenalter TYPE ztvenalter.

  CASE io_event->mv_event_id.
*1 提交
    WHEN 'COTH_SUBMIT'.
*      数据校验
      LOOP AT gt_list INTO DATA(lw_list) WHERE emailadd IS NOT INITIAL.
        zcl_fpm002_basic_06=>m_check_emailadd( EXPORTING i_emailadd = lw_list-emailadd IMPORTING e_result = DATA(lv_result) ).
        IF lv_result <> 'S'.
          CLEAR:lw_message.
          lw_message-msgid = 'ZSRM'.
          lw_message-msgno = '151'.
          lw_message-severity = 'E'.
          lw_message-parameter_1 = lw_list-emailadd.
          APPEND lw_message TO et_messages.
          RETURN.
        ENDIF.
      ENDLOOP.
* 1.1 删除原有的表数据
      DELETE FROM ztvenchgct WHERE alternum = gv_alternum.
* 1.2 取修改编号
      IF gv_alternum IS INITIAL.
        zcl_fpm020_spinfo_change=>m_get_alternum( IMPORTING ev_alternum = gv_alternum ).
      ENDIF.

      MOVE-CORRESPONDING gt_list TO lt_ztvenchgct.
      LOOP AT lt_ztvenchgct INTO lw_ztvenchgct.
        lw_ztvenchgct-alternum = gv_alternum.
        MODIFY lt_ztvenchgct FROM lw_ztvenchgct.
      ENDLOOP.

      lw_ztvenalter-alternum = gv_alternum.
      lw_ztvenalter-altertype = '03'.
      lw_ztvenalter-applydate = sy-datum.
      lw_ztvenalter-applytime = sy-uzeit.
      lw_ztvenalter-applyuser = sy-uname.
      lw_ztvenalter-status = '01'.
      lw_ztvenalter-partnernum = gs_form-partnernum.
      lw_ztvenalter-formalcode = gv_partner.
* 1.3 更新表数据
      MODIFY ztvenalter FROM lw_ztvenalter.
      MODIFY ztvenchgct FROM TABLE lt_ztvenchgct.
      IF sy-subrc = 0.
        CLEAR:lw_message.
        lw_message-msgid = 'ZSRM'.
        lw_message-msgno = '041'.
        lw_message-severity = 'S'.
        APPEND lw_message TO et_messages.
      ENDIF.

*2 接受并更新
    WHEN 'COTH_ACCEPT'.
      CALL METHOD me->m_handle_accept
        IMPORTING
          et_messages = et_messages.

*3 拒绝
    WHEN 'COTH_REJECT'.
      CALL METHOD me->m_handle_reject
        IMPORTING
          et_messages = et_messages.


    WHEN OTHERS.
  ENDCASE.
ENDMETHOD.


  method IF_FPM_GUIBB_LIST~CHECK_CONFIG.
  endmethod.


METHOD if_fpm_guibb_list~flush.
  FIELD-SYMBOLS: <fs_value> TYPE any.
  ASSIGN it_data->* TO <fs_value>.
  gt_list = <fs_value>.

ENDMETHOD.


METHOD if_fpm_guibb_list~get_data.
  DATA: lw_field_usage  TYPE fpmgb_s_fieldusage,
        lw_action_usage TYPE fpmgb_s_actionusage,
        lw_list         TYPE ty_list.

  CASE iv_eventid->mv_event_id.
*1 初始化
    WHEN cl_fpm_event=>gc_event_open_dialog_box.
*1.1 取数初始化
      IF gv_alternum IS NOT INITIAL.
        SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_list
          FROM ztvenchgct
         WHERE alternum = gv_alternum.

      ELSE.
        SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_list
          FROM ztvencoth
         WHERE partnernum = gs_form-partnernum.
      ENDIF.

* 1.2 供应商账号隐藏审批/拒绝按钮；采购员账号隐藏提交按钮；采购员账号不可编辑
      LOOP AT ct_field_usage INTO lw_field_usage.
        IF gv_is_partner IS INITIAL.
          lw_field_usage-read_only = abap_true.
        ENDIF.
        MODIFY ct_field_usage FROM lw_field_usage.
      ENDLOOP.
      LOOP AT ct_action_usage INTO lw_action_usage.
        IF gv_is_partner IS INITIAL.
          lw_action_usage-visible = '01'.
        ENDIF.
        MODIFY ct_action_usage FROM lw_action_usage.
      ENDLOOP.

*1.3 总经理/业务经理/业务联系人/财务联系人 岗位不可编辑
      IF gv_is_partner = 'X'.
        LOOP AT gt_list INTO lw_list.
          IF lw_list-zposition = '01' OR
             lw_list-zposition = '02' OR
             lw_list-zposition = '03' OR
             lw_list-zposition = '04'.
            lw_list-read_only_ref = 'X'.
          ENDIF.
          MODIFY gt_list FROM lw_list.
        ENDLOOP.
      ELSE.
        LOOP AT gt_list INTO lw_list.
          lw_list-read_only_ref = 'X'.
          MODIFY gt_list FROM lw_list.
        ENDLOOP.
      ENDIF.

      ev_field_usage_changed = abap_true.
      ev_action_usage_changed = abap_true.

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
    IF lw_field_descr-name = 'NAME' OR
       lw_field_descr-name = 'PHONENUM' OR
       lw_field_descr-name = 'MOBILEPHONE' OR
       lw_field_descr-name = 'EMAILADD' OR
       lw_field_descr-name = 'ZPOSITION'.
      lw_field_descr-mandatory = abap_true.
    ENDIF.

    IF lw_field_descr-name = 'ZPOSITION'.
      lw_field_descr-read_only_ref = 'READ_ONLY_REF'.
    ENDIF.

    APPEND lw_field_descr TO et_field_description.
    CLEAR lw_field_descr.
  ENDLOOP.

*-----------------------------------------------------------------------
* 设置按钮
*-----------------------------------------------------------------------
  CLEAR lw_action_line.
  lw_action_line-id       = 'COTH_ADD_ROW'.
  lw_action_line-visible  = cl_wd_uielement=>e_visible-visible.
  lw_action_line-enabled  = abap_true.
  lw_action_line-action_type  = 1.
  APPEND lw_action_line TO et_action_definition.
  CLEAR lw_action_line.
  lw_action_line-id       = 'COTH_DEL_ROW'.
  lw_action_line-visible  = cl_wd_uielement=>e_visible-visible.
  lw_action_line-enabled  = abap_true.
  lw_action_line-action_type  = 1.
  APPEND lw_action_line TO et_action_definition.
  CLEAR lw_action_line.
  lw_action_line-id       = 'COTH_SEL_POSITION'.
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


  CASE io_event->mv_event_id.
*1 新增行
    WHEN 'COTH_ADD_ROW'.
      lw_list-alternum = gv_alternum.
      lw_list-partnernum = gs_form-partnernum.
      APPEND lw_list TO gt_list.

*2 删除行
    WHEN 'COTH_DEL_ROW'.
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
      lt_selected_lines = it_selected_lines.
      SORT lt_selected_lines DESCENDING BY tabix.
      LOOP AT it_selected_lines INTO lw_selected_lines.
*        总经理/业务经理/业务联系人/财务联系人 不允许删除
        IF gt_list[ lw_selected_lines-tabix ]-zposition = '01' OR
           gt_list[ lw_selected_lines-tabix ]-zposition = '02' OR
           gt_list[ lw_selected_lines-tabix ]-zposition = '03' OR
           gt_list[ lw_selected_lines-tabix ]-zposition = '04'.
          lt_list = gt_list.
          DELETE lt_list WHERE zposition <> gt_list[ lw_selected_lines-tabix ]-zposition.
          DESCRIBE TABLE lt_list LINES lv_counter.
          IF lv_counter <= 1.
            CLEAR:lw_message.
            lw_message-msgid = 'ZSRM'.
            lw_message-msgno = '026'.
            lw_message-severity = 'E'.
            APPEND lw_message TO et_messages.
            RETURN.
          ENDIF.
        ENDIF.
        DELETE gt_list INDEX lw_selected_lines-tabix.
      ENDLOOP.

*3 选择岗位
    WHEN 'COTH_SEL_POSITION'.
      READ TABLE gt_list INTO lw_list INDEX iv_lead_index.
      IF lw_list-zposition = '01' OR
         lw_list-zposition = '02' OR
         lw_list-zposition = '03' OR
         lw_list-zposition = '04'.
        lw_list-read_only_ref = 'X'.
        MODIFY gt_list FROM lw_list INDEX iv_lead_index.
      ENDIF.

    WHEN OTHERS.
  ENDCASE.
ENDMETHOD.


  method IF_FPM_GUIBB~GET_PARAMETER_LIST.
  endmethod.


METHOD if_fpm_guibb~initialize.


ENDMETHOD.


METHOD m_handle_accept.
  DATA: lw_message    TYPE fpmgb_s_t100_message,
        lt_ztvencoth  TYPE STANDARD TABLE OF ztvencoth,
        lw_ztvenalter TYPE ztvenalter,
        lw_list       TYPE ty_list,
        lt_dd07v      TYPE STANDARD TABLE OF dd07v,
        lw_dd07v      TYPE dd07v.
  DATA: lw_header  TYPE zzvendor_change_header,
        lt_contact TYPE ztvendor_create_contact,
        lw_contact TYPE zzvendor_create_contact,
        lv_msgty   TYPE msgty,
        lv_message TYPE bapi_msg.

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

*2 调用接口更新ECC
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_dd07v
    FROM dd07v
   WHERE domname = 'ZPOSITION'
     AND ddlanguage = '1'.
  SORT lt_dd07v BY domvalue_l.

  lw_header-lifnr = gs_form-partner.
  LOOP AT gt_list INTO lw_list.
    lw_contact-name_last = lw_list-name.
    READ TABLE lt_dd07v INTO lw_dd07v WITH KEY domvalue_l = lw_list-zposition BINARY SEARCH.
    CONCATENATE lw_list-zposition lw_dd07v-ddtext INTO lw_contact-department SEPARATED BY space.
    lw_contact-tel_number = lw_list-phonenum.
    lw_contact-mob_number = lw_list-mobilephone.
    lw_contact-smtp_addr = lw_list-emailadd.
    APPEND lw_contact TO lt_contact.
    CLEAR lw_contact.
  ENDLOOP.
  CALL FUNCTION 'Z_SRM_ECC_CHANGE_VENDOR'
    EXPORTING
      i_chgtype  = '04'
      i_header   = lw_header
      it_contact = lt_contact
    IMPORTING
      e_msgty    = lv_msgty
      e_message  = lv_message.


*3 更新数据库表
  DELETE FROM ztvencoth WHERE partnernum = gs_form-partnernum.
  MOVE-CORRESPONDING gt_list TO lt_ztvencoth.
  MODIFY ztvencoth FROM TABLE lt_ztvencoth.

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
ENDCLASS.
