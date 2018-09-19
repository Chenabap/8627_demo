CLASS zcl_fpm001_initregistration DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_fpm_guibb .
    INTERFACES if_fpm_guibb_form .
    INTERFACES if_fpm_guibb_list .

    TYPES:
      BEGIN OF ty_form ,
        protocol   TYPE char1,
        accept     TYPE char1,
        reject     TYPE char1,
        bnslicense TYPE ztvenstatus-bnslicense,
        date       TYPE sy-datum,
      END OF ty_form .
    TYPES:
      BEGIN OF ty_file,
        zfnumb TYPE ztfile-zfnumb,
        zfname TYPE ztfile-zfname,
      END OF ty_file .
    TYPES:
      ty_t_file TYPE STANDARD TABLE OF ty_file .

    CLASS-DATA gs_form TYPE ty_form .
    CLASS-DATA gt_file TYPE ty_t_file .

    METHODS m_download_file
      IMPORTING
        !i_arc_doc_id TYPE saeardoid .
    METHODS m_generate_partnernum
      EXPORTING
        !partnernum TYPE zpartnernum .
    CLASS-METHODS m_goto_questionnaire
      IMPORTING
        !partnernum        TYPE zpartnernum
        !from_preselection TYPE xfeld OPTIONAL .
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
  DATA: lt_lines  TYPE STANDARD TABLE OF tline,
        lw_lines  TYPE tline,
        lw_action TYPE fpmgb_s_actionusage.

  CASE io_event->mv_event_id.
*      初始状态
    WHEN cl_fpm_event=>gc_event_start.
      DATA docu_utility TYPE REF TO cl_wdr_docu_utility.
      DATA lv_text_xml  TYPE string.

      docu_utility = cl_wdr_docu_utility=>get_instance( ).
      lv_text_xml = docu_utility->get_general_docu_as_fmt_text(
                        object = `ZPROTOCOL` " your document OBJECT FROM se61
                        langu  = sy-langu ).
      gs_form-date = sy-datum.

      cs_data = gs_form.
      ev_data_changed = abap_true.

*      接受协议，按钮可操作
    WHEN 'ACCEPT'.
      LOOP AT ct_action_usage INTO lw_action WHERE id = 'REGISTERED' OR id = 'EDIT'.
        IF gs_form-accept = 'X'.
          lw_action-enabled = abap_true.
        ELSE.
          lw_action-enabled = abap_false.
        ENDIF.
        MODIFY ct_action_usage FROM lw_action.
      ENDLOOP.
      ev_action_usage_changed = abap_true.

    WHEN OTHERS.
  ENDCASE.

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
  DATA: lw_message     TYPE fpmgb_s_t100_message,
        lw_ztvenstatus TYPE ztvenstatus.

  CASE io_event->mv_event_id.
*1 注册
    WHEN 'REGISTERED'.
* 1.1 校验
*      请输入统一社会信用代码（注册登记号）！
      IF gs_form-bnslicense IS INITIAL.
        CLEAR:lw_message.
        lw_message-msgid = 'ZSRM'.
        lw_message-msgno = '001'.
        lw_message-severity = 'E'.
        APPEND lw_message TO et_messages.
        RETURN.
      ENDIF.
*      已注册不允许再次注册！
      SELECT COUNT(*) FROM ztvenstatus
       WHERE bnslicense = gs_form-bnslicense.
      IF sy-subrc = 0.
        CLEAR:lw_message.
        lw_message-msgid = 'ZSRM'.
        lw_message-msgno = '002'.
        lw_message-severity = 'E'.
        APPEND lw_message TO et_messages.
        RETURN.
      ENDIF.

* 1.2 创建SRM供应商编码流水号
      CALL METHOD me->m_generate_partnernum
        IMPORTING
          partnernum = lw_ztvenstatus-partnernum.
      lw_ztvenstatus-bnslicense = gs_form-bnslicense.
      lw_ztvenstatus-regdate = sy-datum.
      lw_ztvenstatus-oareview = '00'.
      MODIFY ztvenstatus FROM lw_ztvenstatus.

*      跳转到调查问卷
      CALL METHOD me->m_goto_questionnaire
        EXPORTING
          partnernum = lw_ztvenstatus-partnernum.


*2 编辑
    WHEN 'EDIT'.
* 2.1 校验
*      请输入统一社会信用代码（注册登记号）！
      IF gs_form-bnslicense IS INITIAL.
        CLEAR:lw_message.
        lw_message-msgid = 'ZSRM'.
        lw_message-msgno = '001'.
        lw_message-severity = 'E'.
        APPEND lw_message TO et_messages.
        RETURN.
      ENDIF.
      SELECT SINGLE * INTO CORRESPONDING FIELDS OF lw_ztvenstatus
        FROM ztvenstatus
       WHERE bnslicense = gs_form-bnslicense.
      IF sy-subrc <> 0.
*      统一社会信用代码（注册登记号）不存在，请检查！
        CLEAR:lw_message.
        lw_message-msgid = 'ZSRM'.
        lw_message-msgno = '003'.
        lw_message-severity = 'E'.
        APPEND lw_message TO et_messages.
        RETURN.

      ELSEIF sy-subrc = 0 AND lw_ztvenstatus-surveystatus = '01'.
*      该调查问卷已提交不允许修改！
        CLEAR:lw_message.
        lw_message-msgid = 'ZSRM'.
        lw_message-msgno = '004'.
        lw_message-severity = 'E'.
        APPEND lw_message TO et_messages.
        RETURN.
      ENDIF.

*      跳转到调查问卷
      CALL METHOD me->m_goto_questionnaire
        EXPORTING
          partnernum = lw_ztvenstatus-partnernum.



    WHEN OTHERS.
  ENDCASE.

ENDMETHOD.


  method IF_FPM_GUIBB_LIST~CHECK_CONFIG.
  endmethod.


  method IF_FPM_GUIBB_LIST~FLUSH.
  endmethod.


METHOD if_fpm_guibb_list~get_data.
  DATA: lw_file TYPE ty_file.

  CASE iv_eventid->mv_event_id.
*      初始状态
    WHEN cl_fpm_event=>gc_event_start.
      SELECT
        zttemplatefile~zfnumb
        ztfile~zfname
        INTO CORRESPONDING FIELDS OF TABLE gt_file
        FROM zttemplatefile
        JOIN ztfile
          ON zttemplatefile~zfnumb = ztfile~zfnumb
       WHERE zttemplatefile~templatetype = '03'   "调查问卷填写操作手册
          OR zttemplatefile~templatetype = '04'.  "通知平台操作手册

      DATA lo_fpm TYPE REF TO if_fpm.
      lo_fpm = cl_fpm_factory=>get_instance( ).

      ct_data = gt_file.
      ev_data_changed = abap_true.

    WHEN OTHERS.
  ENDCASE.

ENDMETHOD.


  method IF_FPM_GUIBB_LIST~GET_DEFAULT_CONFIG.
  endmethod.


METHOD IF_FPM_GUIBB_LIST~GET_DEFINITION.
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

  lo_struct_descr ?= cl_abap_structdescr=>describe_by_name( 'TY_FILE' ).
  eo_field_catalog = cl_abap_tabledescr=>create( lo_struct_descr ).
  lt_comp_tab = lo_struct_descr->get_components( ).


*展示字段
  LOOP AT lt_comp_tab INTO lw_comp_tab WHERE name IS NOT INITIAL.
    lw_field_descr-name = lw_comp_tab-name.
    lw_field_descr-DEFAULT_DISPLAY_TYPE = 'LA'.   "到操作的链接
    lw_field_descr-header_label = text-006.
    APPEND lw_field_descr TO et_field_description.
    CLEAR lw_field_descr.

  ENDLOOP.
ENDMETHOD.


METHOD IF_FPM_GUIBB_LIST~PROCESS_EVENT.
  DATA: lw_file TYPE ty_file.

  CASE io_event->mv_event_id.
    WHEN 'FPM_GUIBB_LIST_CELL_ACTION'.
      READ TABLE gt_file INTO lw_file INDEX iv_event_index.
      IF sy-subrc = 0.
        CALL METHOD me->m_download_file
          EXPORTING
            i_arc_doc_id = lw_file-zfnumb.

      ENDIF.
    WHEN OTHERS.
  ENDCASE.


ENDMETHOD.


  method IF_FPM_GUIBB~GET_PARAMETER_LIST.
  endmethod.


METHOD IF_FPM_GUIBB~INITIALIZE.

ENDMETHOD.


METHOD M_DOWNLOAD_FILE.
  DATA: lv_file     TYPE xstring,
        lv_filename TYPE string,
        lv_filetype TYPE string.

  CALL FUNCTION 'ZSCMS_READ_DOWNLOAD'
    EXPORTING
*     SAP_OBJECT =
      arc_doc_id = i_arc_doc_id
    IMPORTING
      file       = lv_file
      filename   = lv_filename
      mime_type  = lv_filetype.

  CALL METHOD cl_wd_runtime_services=>attach_file_to_response
    EXPORTING
      i_filename  = lv_filename
      i_content   = lv_file
      i_mime_type = lv_filetype.



ENDMETHOD.


METHOD M_GENERATE_PARTNERNUM.
  CALL FUNCTION 'NUMBER_RANGE_ENQUEUE'
    EXPORTING
      object           = 'ZPARTNER'
    EXCEPTIONS
      foreign_lock     = 1
      object_not_found = 2
      system_failure   = 3
      OTHERS           = 4.
*  **如果号码范围存在
  IF sy-subrc EQ 0 .
*  ****得到一个号码，
    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = '01'
        object                  = 'ZPARTNER'
      IMPORTING
        number                  = partnernum
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.
*  ***将号码累加
    CALL FUNCTION 'NUMBER_RANGE_DEQUEUE'
      EXPORTING
        object           = 'ZPARTNER'
      EXCEPTIONS
        object_not_found = 1
        OTHERS           = 2.
  ENDIF .

ENDMETHOD.


METHOD m_goto_questionnaire.
  DATA: lv_app_name    TYPE string,
        lv_in_protocol TYPE string,
        lv_wd_url      TYPE string,
        lv_host        TYPE string,
        lv_port        TYPE string,
        lv_langu       TYPE string,
        lt_tihttpnvp   TYPE tihttpnvp,
        lw_tihttpnvp   TYPE ihttpnvp,
        ls_tihttpnvp   LIKE LINE OF lt_tihttpnvp,
        ls_httpurlloc  TYPE httpurlloc.
  DATA: lw_url_fields TYPE fpm_s_launch_url.

  CLEAR:ls_httpurlloc.
  SELECT SINGLE *
  INTO ls_httpurlloc
  FROM httpurlloc.

*设定参数值
  lv_in_protocol = ls_httpurlloc-protocol.
  lv_host = ls_httpurlloc-host.
  lv_port = ls_httpurlloc-port.

  IF sy-langu EQ '1'.
    lv_langu = 'ZH'.
  ENDIF.
  IF sy-langu EQ 'E'.
    lv_langu = 'EN'.
  ENDIF.

  "Client
  lw_tihttpnvp-name = 'sap-client'.
  lw_tihttpnvp-value = sy-mandt.
  APPEND lw_tihttpnvp TO lt_tihttpnvp.
  CLEAR lw_tihttpnvp.

  "Language
  lw_tihttpnvp-name = 'sap-language'.
  lw_tihttpnvp-value = lv_langu.
  APPEND lw_tihttpnvp TO lt_tihttpnvp.
  CLEAR lw_tihttpnvp.

  "Config
  lv_app_name = 'ZFPM002_QUESTIONNAIRE'.
  lw_tihttpnvp-value = 'ZFPM002_QUESTIONNAIRE_ID'.
  lw_tihttpnvp-name = 'sap-wd-configId'.
  APPEND lw_tihttpnvp TO lt_tihttpnvp.
  CLEAR lw_tihttpnvp.


  "PARTNERNUM
  lw_tihttpnvp-name = 'PARTNERNUM'.
  lw_tihttpnvp-value = partnernum.
  APPEND lw_tihttpnvp TO lt_tihttpnvp.
  CLEAR lw_tihttpnvp.

  "FROM_PRESELECTION
  IF from_preselection = 'X'.
    lw_tihttpnvp-name = 'FROM_PRESELECTION'.
    lw_tihttpnvp-value = 'X'.
    APPEND lw_tihttpnvp TO lt_tihttpnvp.
    CLEAR lw_tihttpnvp.
  ENDIF.




  "重新生成URL
  CALL METHOD cl_wd_utilities=>construct_wd_url
    EXPORTING
      application_name = lv_app_name
      in_host          = lv_host
      in_port          = lv_port
      in_protocol      = lv_in_protocol
      in_parameters    = lt_tihttpnvp
    IMPORTING
      out_absolute_url = lv_wd_url.

  lw_url_fields-url = lv_wd_url.
  CALL METHOD y_cl_wd_tool=>navigate_externalurl
    EXPORTING
      is_url_fields = lw_url_fields.


ENDMETHOD.
ENDCLASS.
