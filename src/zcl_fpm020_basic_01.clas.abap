CLASS zcl_fpm020_basic_01 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_fpm_guibb .
    INTERFACES if_fpm_guibb_form .
    INTERFACES if_fpm_guibb_list .

    TYPES:
      BEGIN OF ty_form,
        alternum         TYPE ztvenalter-alternum,
        name_org1        TYPE but000-name_org1,
        status           TYPE ztvenalter-status,
        partnernum       TYPE ztbasic1-partnernum,
        partner          TYPE but000-partner,
        applyuser        TYPE ztvenalter-applyuser,
        operation        TYPE char2,
        ekorg            TYPE ztvenchgbasic-ekorg,
        exvenrank        TYPE ztvenchgbasic-exvenrank,
        venrank          TYPE ztvenchgbasic-venrank,
        exname1          TYPE ztvenchgbasic-exname1,
        exname2          TYPE ztvenchgbasic-exname2,
        exsimplename     TYPE ztvenchgbasic-exsimplename,
        exvenaddress     TYPE ztvenchgbasic-exvenaddress,
        exbnslicense     TYPE ztvenchgbasic-exbnslicense,
        exbanknation     TYPE ztvenchgbasic-exbanknation,
        exbankname       TYPE ztvenchgbasic-exbankname,
        exbankaccount    TYPE ztvenchgbasic-exbankaccount,
        exbankowner      TYPE ztvenchgbasic-exbankowner,
        exbankkey        TYPE ztvenchgbasic-exbankkey,
        exbankkey1       TYPE ztvenchgbasic-exbankkey1,
        excurrency       TYPE ztvenchgbasic-excurrency,
        expaymentterm    TYPE ztvenchgbasic-expaymentterm,
        exincoterms      TYPE ztvenchgbasic-exincoterms,
        exincotermadd    TYPE ztvenchgbasic-exincotermadd,
        exmeprf          TYPE ztvenchgbasic-exmeprf,
        name1            TYPE ztvenchgbasic-name1,
        name2            TYPE ztvenchgbasic-name2,
        simplename       TYPE ztvenchgbasic-simplename,
        venaddress       TYPE ztvenchgbasic-venaddress,
        bnslicense       TYPE ztvenchgbasic-bnslicense,
        banknation       TYPE ztvenchgbasic-banknation,
        bankname         TYPE ztvenchgbasic-bankname,
        bankaccount      TYPE ztvenchgbasic-bankaccount,
        bankowner        TYPE ztvenchgbasic-bankowner,
        bankkey          TYPE ztvenchgbasic-bankkey,
        bankkey1         TYPE ztvenchgbasic-bankkey1,
        currency         TYPE ztvenchgbasic-currency,
        paymentterm      TYPE ztvenchgbasic-paymentterm,
        incoterms        TYPE ztvenchgbasic-incoterms,
        incotermadd      TYPE ztvenchgbasic-incotermadd,
        meprf            TYPE ztvenchgbasic-meprf,
        name1_flag       TYPE xfeld,
        name2_flag       TYPE xfeld,
        simplename_flag  TYPE xfeld,
        venaddress_flag  TYPE xfeld,
        bnslicense_flag  TYPE xfeld,
        banknation_flag  TYPE xfeld,
        bankname_flag    TYPE xfeld,
        bankaccount_flag TYPE xfeld,
        bankowner_flag   TYPE xfeld,
        bankkey_flag     TYPE xfeld,
        bankkey1_flag    TYPE xfeld,
        currency_flag    TYPE xfeld,
        paymentterm_flag TYPE xfeld,
        incoterms_flag   TYPE xfeld,
        incotermadd_flag TYPE xfeld,
        meprf_flag       TYPE xfeld,
      END OF ty_form .
    TYPES:
      BEGIN OF ty_list,
        alternum    TYPE ztvenfile3-alternum,
        filetype    TYPE ztvenfile3-filetype,
        exzfnumb    TYPE ztvenfile3-exzfnumb,
        exzfname    TYPE ztfile-zfname,
        exstartdate TYPE ztvenfile3-exstartdate,
        exdeadline  TYPE ztvenfile3-exdeadline,
        if_change   TYPE ztvenfile3-if_change,
        zfnumb      TYPE ztvenfile3-zfnumb,
        zfname      TYPE ztfile-zfname,
        startdate   TYPE ztvenfile3-startdate,
        deadline    TYPE ztvenfile3-deadline,
        upload      TYPE char20,
        change_ref  TYPE char1,
        zfdesc      TYPE ztfile-zfdesc,
        zftype      TYPE ztfile-zftype,
        zfsize      TYPE ztfile-zfsize,
        zfsizm      TYPE ztfile-zfsizm,
        zfsizk      TYPE ztfile-zfsizk,
        zfpath      TYPE ztfile-zfpath,
        zfile       TYPE ztfile-zfile,
        zdatum      TYPE ztfile-zdatum,
        zuzeit      TYPE ztfile-zuzeit,
        zcrdby      TYPE ztfile-zcrdby,
        change_file TYPE char1,
        enabled_ref TYPE char1,
      END OF ty_list .
    TYPES:
      ty_t_list TYPE STANDARD TABLE OF ty_list .

    DATA gs_form TYPE ty_form .
    CLASS-DATA mo_shared_data TYPE REF TO y_cl_wd_shared_data .
    DATA gv_is_partner TYPE char1 .
    DATA gv_alternum TYPE ztvenalter-alternum .
    DATA gv_partner TYPE but000-partner .
    DATA gt_list TYPE ty_t_list .
    DATA gv_event_index TYPE i .

    METHODS m_download_file
      IMPORTING
        !iv_zfnumb    TYPE saeardoid OPTIONAL
        !iv_zfile     TYPE ztfile-zfile OPTIONAL
        !iv_filename  TYPE ztfile-zfname OPTIONAL
        !iv_mine_type TYPE ztfile-zftype OPTIONAL .
    METHODS m_handle_submit
      EXPORTING
        !et_messages TYPE fpmgb_t_messages .
    METHODS m_handle_reject
      EXPORTING
        !et_messages TYPE fpmgb_t_messages .
    METHODS m_handle_update
      EXPORTING
        !et_messages TYPE fpmgb_t_messages .
    METHODS m_handle_approval
      EXPORTING
        !et_messages TYPE fpmgb_t_messages .
    METHODS m_set_visible_by_operation
      IMPORTING
        !read_only          TYPE xfeld
      CHANGING
        !ct_list_fieldusage TYPE fpmgb_t_fieldusage OPTIONAL
        !ct_form_fieldusage TYPE fpmgb_t_fieldusage OPTIONAL .
    METHODS m_set_visible_by_status
      IMPORTING
        !alternum           TYPE ztvenalter-alternum OPTIONAL
      CHANGING
        !ct_list_fieldusage TYPE fpmgb_t_fieldusage OPTIONAL
        !ct_form_fieldusage TYPE fpmgb_t_fieldusage OPTIONAL
        !ct_action          TYPE fpmgb_t_actionusage OPTIONAL .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_FPM020_BASIC_01 IMPLEMENTATION.


  METHOD if_fpm_guibb_form~check_config.
  ENDMETHOD.


  METHOD if_fpm_guibb_form~flush.
    DATA: lv_detailtype TYPE ztbasic1-detailtype.

    FIELD-SYMBOLS: <fs_value> TYPE any.
    ASSIGN is_data->* TO <fs_value>.

    gs_form = <fs_value>.

  ENDMETHOD.


  METHOD if_fpm_guibb_form~get_data.
    DATA: lw_field_usage TYPE fpmgb_s_fieldusage.


    CASE io_event->mv_event_id.
*1 初始化
      WHEN cl_fpm_event=>gc_event_open_dialog_box.
        CLEAR: gv_is_partner,gv_partner,gv_alternum,gs_form,gt_list.
*  实例化共享类
        mo_shared_data ?= y_cl_wd_shared_data=>get_instance( ).
        mo_shared_data->if_fpm_parameter~get_value( EXPORTING iv_key = 'IS_PARTNER' IMPORTING ev_value = gv_is_partner ).
        mo_shared_data->if_fpm_parameter~get_value( EXPORTING iv_key = 'PARTNER' IMPORTING ev_value = gv_partner ).
        mo_shared_data->if_fpm_parameter~get_value( EXPORTING iv_key = 'ALTERNUM' IMPORTING ev_value = gv_alternum ).
        mo_shared_data->if_fpm_parameter~delete_value( iv_key = 'IS_PARTNER').
        mo_shared_data->if_fpm_parameter~delete_value( iv_key = 'PARTNER').
        mo_shared_data->if_fpm_parameter~delete_value( iv_key = 'ALTERNUM').

        SELECT SINGLE partnernum FROM ztvenstatus INTO gs_form-partnernum WHERE formalcode = gv_partner.
        IF gv_alternum IS INITIAL.
          gs_form-alternum = '#########'.
          gs_form-applyuser = sy-uname.

          SELECT SINGLE
          name1 AS exname1
          name2 AS exname2
          simplename AS exsimplename
          venaddress AS exvenaddress
          INTO CORRESPONDING FIELDS OF gs_form
          FROM ztbasic1
         WHERE partnernum = gs_form-partnernum.
          SELECT SINGLE
            venrank AS exvenrank
            bnslicense AS exbnslicense
            INTO CORRESPONDING FIELDS OF gs_form
            FROM ztvenstatus
           WHERE partnernum = gs_form-partnernum.
          SELECT SINGLE
            banknation AS exbanknation
            bankname AS exbankname
            bankaccount AS exbankaccount
            bankowner AS exbankowner
            bankkey AS exbankkey
            bankkey1 AS exbankkey1
            currency AS excurrency
            paymentterm AS expaymentterm
            incoterms AS exincoterms
            incotermadd AS exincotermadd
            INTO CORRESPONDING FIELDS OF gs_form
            FROM ztvenfi
           WHERE partnernum = gs_form-partnernum.
          SELECT SINGLE
            meprf AS exmeprf
            INTO CORRESPONDING FIELDS OF gs_form
            FROM ztvencreate
           WHERE partnernum = gs_form-partnernum.

          gs_form-ekorg = '2000'.
          gs_form-name1 = gs_form-exname1.
          gs_form-name2 = gs_form-exname2.
          gs_form-simplename = gs_form-exsimplename.
          gs_form-venaddress = gs_form-exvenaddress.
          gs_form-bnslicense = gs_form-exbnslicense.
          gs_form-banknation = gs_form-exbanknation.
          gs_form-bankname = gs_form-exbankname.
          gs_form-bankaccount = gs_form-exbankaccount.
          gs_form-bankowner = gs_form-exbankowner.
          gs_form-bankkey = gs_form-exbankkey.
          gs_form-bankkey1 = gs_form-exbankkey1.
          gs_form-currency = gs_form-excurrency.
          gs_form-paymentterm = gs_form-expaymentterm.
          gs_form-incoterms = gs_form-exincoterms.
          gs_form-incotermadd = gs_form-exincotermadd.
          gs_form-meprf = gs_form-exmeprf.


        ELSE.
          SELECT SINGLE * INTO CORRESPONDING FIELDS OF gs_form
            FROM ztvenalter
           WHERE alternum = gv_alternum.
          SELECT SINGLE * INTO CORRESPONDING FIELDS OF gs_form
            FROM ztvenchgbasic
           WHERE alternum = gv_alternum.
        ENDIF.

        gs_form-partner = gv_partner.
        IF gv_is_partner = 'X'.
          gs_form-operation = '02'.
        ENDIF.
        SELECT SINGLE name_org1 FROM but000 INTO gs_form-name_org1 WHERE partner = gv_partner.

        IF gs_form-exname1 <> gs_form-name1.
          gs_form-name1_flag = abap_true.
        ENDIF.
        IF gs_form-exname2 <> gs_form-name2.
          gs_form-name2_flag = abap_true.
        ENDIF.
        IF gs_form-exsimplename <> gs_form-simplename.
          gs_form-simplename_flag = abap_true.
        ENDIF.
        IF gs_form-exvenaddress <> gs_form-venaddress.
          gs_form-venaddress_flag = abap_true.
        ENDIF.
        IF gs_form-exbnslicense <> gs_form-bnslicense.
          gs_form-bnslicense_flag = abap_true.
        ENDIF.
        IF gs_form-exbanknation <> gs_form-banknation.
          gs_form-banknation_flag = abap_true.
        ENDIF.
        IF gs_form-exbankname <> gs_form-bankname.
          gs_form-bankname_flag = abap_true.
        ENDIF.
        IF gs_form-exbankaccount <> gs_form-bankaccount.
          gs_form-bankaccount_flag = abap_true.
        ENDIF.
        IF gs_form-exbankowner <> gs_form-bankowner.
          gs_form-bankowner_flag = abap_true.
        ENDIF.
        IF gs_form-exbankkey <> gs_form-bankkey.
          gs_form-bankkey_flag = abap_true.
        ENDIF.
        IF gs_form-exbankkey1 <> gs_form-bankkey1.
          gs_form-bankkey1_flag = abap_true.
        ENDIF.
        IF gs_form-excurrency <> gs_form-currency.
          gs_form-currency_flag = abap_true.
        ENDIF.
        IF gs_form-expaymentterm <> gs_form-paymentterm.
          gs_form-paymentterm_flag = abap_true.
        ENDIF.
        IF gs_form-exincoterms <> gs_form-incoterms.
          gs_form-incoterms_flag = abap_true.
        ENDIF.
        IF gs_form-exincotermadd <> gs_form-incotermadd.
          gs_form-incotermadd_flag = abap_true.
        ENDIF.
        IF gs_form-exmeprf <> gs_form-meprf.
          gs_form-meprf_flag = abap_true.
        ENDIF.



        IF gv_is_partner = 'X'.
          TRY.
              ct_field_usage[ name = 'EKORG' ]-visibility = '01'.
              ct_field_usage[ name = 'OPERATION' ]-read_only = abap_true.
            CATCH  cx_sy_itab_line_not_found .
          ENDTRY.
        ENDIF.
        LOOP AT ct_field_usage INTO lw_field_usage WHERE name = 'EXVENRANK' OR name = 'VENRANK'.
          IF gs_form-operation <> '05'.
            lw_field_usage-visibility = '01'.
          ELSE.
            lw_field_usage-visibility = '02'.
          ENDIF.
          MODIFY ct_field_usage FROM lw_field_usage.
        ENDLOOP.


*        IF gs_form-operation <> '02'.
*          ct_action_usage[ id = 'BASIC_SUBMIT' ]-visible = '01'.
*        ENDIF.
        IF gv_is_partner = 'X'.
          ct_action_usage[ id = 'BASIC_REJECT' ]-visible = '01'.
          ct_action_usage[ id = 'BASIC_UPDATE' ]-visible = '01'.
          ct_action_usage[ id = 'BASIC_APPROVAL' ]-visible = '01'.
        ENDIF.


* 1.2 属性设置
*  1.2.1 根据操作类型设置可编辑属性
        IF gs_form-operation = '02' OR gs_form-operation IS INITIAL.
          SELECT COUNT(*) FROM ztvenalter WHERE alternum = gv_alternum AND status <> '01' AND status <> '03' AND status <> '05'.
          IF sy-subrc = 0.
            me->m_set_visible_by_operation( EXPORTING read_only = abap_true CHANGING ct_form_fieldusage = ct_field_usage ).
          ELSE.
            me->m_set_visible_by_operation( EXPORTING read_only = abap_false CHANGING ct_form_fieldusage = ct_field_usage ).
          ENDIF.
        ELSE.
          me->m_set_visible_by_operation( EXPORTING read_only = abap_true CHANGING ct_form_fieldusage = ct_field_usage ).
        ENDIF.
*  1.2.2 设置操作字段可编辑属性
        IF gv_is_partner IS INITIAL.
          IF gv_alternum IS INITIAL.
            ct_field_usage[ name = 'OPERATION' ]-read_only = abap_false.
          ELSE.
            SELECT COUNT(*) FROM ztvenalter WHERE alternum = gv_alternum
                                              AND ( status = '04' OR status = '06' OR status = '07' OR status = '08').
            IF sy-subrc = 0.
              ct_field_usage[ name = 'OPERATION' ]-read_only = abap_true.
            ELSE.
              ct_field_usage[ name = 'OPERATION' ]-read_only = abap_false.
            ENDIF.
          ENDIF.
        ENDIF.
*  1.2.3 根据状态设置可编辑
        me->m_set_visible_by_status( EXPORTING alternum = gv_alternum CHANGING ct_form_fieldusage = ct_field_usage ct_action = ct_action_usage ).

        ev_action_usage_changed = abap_true.
        ev_field_usage_changed = abap_true.

*2 变更操作类型
      WHEN 'CHANGE_OPERATION'.

        LOOP AT ct_field_usage INTO lw_field_usage WHERE name = 'EXVENRANK' OR name = 'VENRANK'.
          IF gs_form-operation <> '05'.
            lw_field_usage-visibility = '01'.
          ELSE.
            lw_field_usage-visibility = '02'.
          ENDIF.
          MODIFY ct_field_usage FROM lw_field_usage.
        ENDLOOP.

        IF gs_form-operation = '02'.
          me->m_set_visible_by_operation( EXPORTING read_only = abap_false CHANGING ct_form_fieldusage = ct_field_usage ).
        ELSE.
          me->m_set_visible_by_operation( EXPORTING read_only = abap_true CHANGING ct_form_fieldusage = ct_field_usage ).
        ENDIF.
        ev_field_usage_changed = abap_true.

*3 提交
      WHEN 'BASIC_SUBMIT'.
        me->m_set_visible_by_status( EXPORTING alternum = gv_alternum CHANGING ct_form_fieldusage = ct_field_usage ct_action = ct_action_usage ).
        ev_action_usage_changed = abap_true.
        ev_field_usage_changed = abap_true.
*4 拒绝
      WHEN 'BASIC_REJECT'.
        me->m_set_visible_by_status( EXPORTING alternum = gv_alternum CHANGING ct_form_fieldusage = ct_field_usage ct_action = ct_action_usage ).
        ev_action_usage_changed = abap_true.
        ev_field_usage_changed = abap_true.
*5 更新
      WHEN 'BASIC_UPDATE'.
        me->m_set_visible_by_status( EXPORTING alternum = gv_alternum CHANGING ct_form_fieldusage = ct_field_usage ct_action = ct_action_usage ).
        ev_action_usage_changed = abap_true.
        ev_field_usage_changed = abap_true.
*6 发起审批
      WHEN 'BASIC_APPROVAL'.
        me->m_set_visible_by_status( EXPORTING alternum = gv_alternum CHANGING ct_form_fieldusage = ct_field_usage ct_action = ct_action_usage ).
        ev_action_usage_changed = abap_true.
        ev_field_usage_changed = abap_true.
      WHEN OTHERS.
    ENDCASE.


    cs_data = gs_form.
    ev_data_changed = abap_true.


  ENDMETHOD.


  METHOD if_fpm_guibb_form~get_default_config.
  ENDMETHOD.


  METHOD if_fpm_guibb_form~get_definition.
    DATA:lw_field_descr TYPE fpmgb_s_formfield_descr.
    DATA:lt_comp_tab    TYPE abap_component_tab.
    DATA:lw_comp_tab    TYPE abap_componentdescr.
    DATA:lw_action_line TYPE fpmgb_s_actiondef.
    DATA: lt_operation    TYPE wdr_context_attr_value_list.
    DATA lt_venrank TYPE wdr_context_attr_value_list.
    DATA lt_currency TYPE wdr_context_attr_value_list.
    DATA lt_incoterms TYPE wdr_context_attr_value_list.
    DATA lt_paymentterm TYPE wdr_context_attr_value_list.
    DATA lt_banknation TYPE wdr_context_attr_value_list.
    DATA lt_meprf TYPE wdr_context_attr_value_list.
    DATA lw_fix_value TYPE wdr_context_attr_value.

    eo_field_catalog ?=  cl_abap_structdescr=>describe_by_name( 'TY_FORM' ).
    lt_comp_tab = eo_field_catalog->get_components( ).


*  操作类型
    lt_operation = VALUE #( ( value = '02' text = text-001 )
                            ( value = '03' text = text-002 )
                            ( value = '04' text = text-003 )
                            ( value = '05' text = text-004 ) ).

    SELECT
      domvalue_l AS value
      ddtext AS text
      INTO CORRESPONDING FIELDS OF TABLE lt_venrank
      FROM dd07v
     WHERE domname = 'ZVENRANK'
       AND ( domvalue_l = '02' OR domvalue_l = '03' )
       AND ddlanguage = sy-langu.


    SELECT waers AS value ltext AS text INTO CORRESPONDING FIELDS OF TABLE lt_currency FROM tcurt WHERE spras = sy-langu.
    LOOP AT lt_currency INTO lw_fix_value.
      CONCATENATE lw_fix_value-value lw_fix_value-text INTO lw_fix_value-text SEPARATED BY space.
      MODIFY lt_currency FROM lw_fix_value.
    ENDLOOP.
    SORT lt_currency BY value.

    SELECT bbpc_incoterms~incoterm_key AS value
           bbpc_incoterms_t~description AS text
    INTO CORRESPONDING FIELDS OF TABLE lt_incoterms
    FROM bbpc_incoterms
    JOIN bbpc_incoterms_t
      ON bbpc_incoterms~incoterm_key = bbpc_incoterms_t~incoterm_key
   WHERE bbpc_incoterms_t~language = sy-langu.
    LOOP AT lt_incoterms INTO lw_fix_value.
      CONCATENATE lw_fix_value-value lw_fix_value-text INTO lw_fix_value-text SEPARATED BY space.
      MODIFY lt_incoterms FROM lw_fix_value.
    ENDLOOP.
    SORT lt_incoterms.

    SELECT zterm AS value text1 AS text
      INTO CORRESPONDING FIELDS OF TABLE lt_paymentterm
      FROM bbp_payterm_text
     WHERE spras = sy-langu.
    SORT lt_paymentterm.
    LOOP AT lt_paymentterm INTO lw_fix_value.
      CONCATENATE lw_fix_value-value lw_fix_value-text INTO lw_fix_value-text SEPARATED BY space.
      MODIFY lt_paymentterm FROM lw_fix_value.
    ENDLOOP.

    SELECT land1 AS value landx AS text
      INTO CORRESPONDING FIELDS OF TABLE lt_banknation
      FROM t005t
     WHERE spras = sy-langu.
    LOOP AT lt_banknation INTO lw_fix_value.
      CONCATENATE lw_fix_value-value lw_fix_value-text INTO lw_fix_value-text SEPARATED BY space.
      MODIFY lt_banknation FROM lw_fix_value.
    ENDLOOP.
    SORT lt_banknation.

    SELECT
      domvalue_l AS value
      ddtext AS text
      INTO CORRESPONDING FIELDS OF TABLE lt_meprf
      FROM dd07v
     WHERE domname = 'ZZMEPRF'
       AND ddlanguage = sy-langu.
    LOOP AT lt_meprf INTO lw_fix_value.
      CONCATENATE lw_fix_value-value lw_fix_value-text INTO lw_fix_value-text SEPARATED BY space.
      MODIFY lt_meprf FROM lw_fix_value.
    ENDLOOP.


    LOOP AT lt_comp_tab INTO lw_comp_tab.
      lw_field_descr-name = lw_comp_tab-name.
      IF lw_field_descr-name = 'OPERATION'.
        lw_field_descr-fixed_values = lt_operation.
      ENDIF.

      CASE lw_field_descr-name.
        WHEN 'ALTERNUM'.
          lw_field_descr-label_text = text-005.
          lw_field_descr-read_only = abap_true.
        WHEN 'NAME_ORG1'.
          lw_field_descr-label_text = text-006.
          lw_field_descr-read_only = abap_true.
        WHEN 'STATUS'.
          lw_field_descr-label_text = text-010.
          lw_field_descr-read_only = abap_true.
        WHEN 'PARTNERNUM'.
          lw_field_descr-label_text = text-007.
          lw_field_descr-read_only = abap_true.
        WHEN 'PARTNER'.
          lw_field_descr-label_text = text-008.
          lw_field_descr-read_only = abap_true.
        WHEN 'APPLYUSER'.
          lw_field_descr-label_text = text-009.
          lw_field_descr-read_only = abap_true.
        WHEN 'OPERATION'.
          lw_field_descr-label_text = text-011.
          IF gv_is_partner = 'X'.
            lw_field_descr-read_only = abap_true.
          ENDIF.
        WHEN 'EXVENRANK'.
          lw_field_descr-label_text = text-012.
          lw_field_descr-read_only = abap_true.
        WHEN 'VENRANK'.
          lw_field_descr-label_text = text-013.
          lw_field_descr-fixed_values = lt_venrank.

        WHEN 'EXNAME1'.
          lw_field_descr-label_text = text-018.
        WHEN 'EXNAME2'.
          lw_field_descr-label_text = text-019.
        WHEN 'EXBNSLICENSE'.
          lw_field_descr-label_text = text-020.
        WHEN 'EXBANKNATION'.
          lw_field_descr-label_text = text-021.
          lw_field_descr-fixed_values = lt_banknation.
        WHEN 'EXBANKNAME'.
          lw_field_descr-label_text = text-022.
        WHEN 'EXBANKOWNER'.
          lw_field_descr-label_text = text-023.
        WHEN 'EXCURRENCY'.
          lw_field_descr-label_text = text-024.
          lw_field_descr-fixed_values = lt_currency.
        WHEN 'EXPAYMENTTERM'.
          lw_field_descr-label_text = text-025.
          lw_field_descr-fixed_values = lt_paymentterm.
        WHEN 'EXINCOTERMS'.
          lw_field_descr-label_text = text-026.
          lw_field_descr-fixed_values = lt_incoterms.
        WHEN 'EXINCOTERMADD'.
          lw_field_descr-label_text = text-027.
        WHEN 'EXMEPRF'.
          lw_field_descr-label_text = text-028.
          lw_field_descr-fixed_values = lt_meprf.
        WHEN 'EKORG'.
          lw_field_descr-mandatory = abap_true.
        WHEN 'CURRENCY'.
          lw_field_descr-fixed_values = lt_currency.
        WHEN 'INCOTERMS'.
          lw_field_descr-fixed_values = lt_incoterms.
        WHEN 'PAYMENTTERM'.
          lw_field_descr-fixed_values = lt_paymentterm.
        WHEN 'BANKNATION'.
          lw_field_descr-fixed_values = lt_banknation.
        WHEN 'MEPRF'.
          lw_field_descr-fixed_values = lt_meprf.
        WHEN OTHERS.
      ENDCASE.

      IF lw_field_descr-name = 'EXNAME1' OR
         lw_field_descr-name = 'EXNAME2' OR
         lw_field_descr-name = 'EXSIMPLENAME' OR
         lw_field_descr-name = 'EXVENADDRESS' OR
         lw_field_descr-name = 'EXBNSLICENSE' OR
         lw_field_descr-name = 'EXBANKNATION' OR
         lw_field_descr-name = 'EXBANKNAME' OR
         lw_field_descr-name = 'EXBANKACCOUNT' OR
         lw_field_descr-name = 'EXBANKOWNER' OR
         lw_field_descr-name = 'EXBANKKEY' OR
         lw_field_descr-name = 'EXBANKKEY1' OR
         lw_field_descr-name = 'EXCURRENCY' OR
         lw_field_descr-name = 'EXPAYMENTTERM' OR
         lw_field_descr-name = 'EXINCOTERMS' OR
         lw_field_descr-name = 'EXINCOTERMADD' OR
         lw_field_descr-name = 'EXMEPRF' .
        lw_field_descr-read_only = abap_true.
      ENDIF.

      IF lw_field_descr-name = 'NAME1' OR
         lw_field_descr-name = 'SIMPLENAME' OR
         lw_field_descr-name = 'VENADDRESS' OR
         lw_field_descr-name = 'BNSLICENSE' OR
         lw_field_descr-name = 'BANKNATION' OR
         lw_field_descr-name = 'BANKNAME' OR
         lw_field_descr-name = 'BANKACCOUNT' OR
         lw_field_descr-name = 'BANKOWNER' OR
         lw_field_descr-name = 'BANKKEY' OR
         lw_field_descr-name = 'BANKKEY1' OR
         lw_field_descr-name = 'CURRENCY' OR
         lw_field_descr-name = 'PAYMENTTERM' OR
         lw_field_descr-name = 'INCOTERMS' OR
         lw_field_descr-name = 'INCOTERMADD' OR
         lw_field_descr-name = 'MEPRF' OR
         lw_field_descr-name = 'OPERATION'.
        lw_field_descr-mandatory = abap_true.
      ENDIF.

      IF lw_field_descr-name =  'NAME1_FLAG' OR
          lw_field_descr-name = 'NAME2_FLAG' OR
          lw_field_descr-name = 'SIMPLENAME_FLAG' OR
          lw_field_descr-name = 'VENADDRESS_FLAG' OR
          lw_field_descr-name = 'BNSLICENSE_FLAG' OR
          lw_field_descr-name = 'BANKNATION_FLAG' OR
          lw_field_descr-name = 'BANKNAME_FLAG' OR
          lw_field_descr-name = 'BANKACCOUNT_FLAG' OR
          lw_field_descr-name = 'BANKOWNER_FLAG' OR
          lw_field_descr-name = 'BANKKEY_FLAG' OR
          lw_field_descr-name = 'BANKKEY1_FLAG' OR
          lw_field_descr-name = 'CURRENCY_FLAG' OR
          lw_field_descr-name = 'PAYMENTTERM_FLAG' OR
          lw_field_descr-name = 'INCOTERMS_FLAG' OR
          lw_field_descr-name = 'INCOTERMADD_FLAG' OR
          lw_field_descr-name = 'MEPRF_FLAG'.
        lw_field_descr-read_only = abap_true.
      ENDIF.

      APPEND lw_field_descr TO et_field_description.
      CLEAR lw_field_descr.
    ENDLOOP.

*-----------------------------------------------------------------------
* 设置按钮
*-----------------------------------------------------------------------
    CLEAR lw_action_line.
    lw_action_line-id       = 'CHANGE_OPERATION'.
    lw_action_line-action_type = '01'.
    APPEND lw_action_line TO et_action_definition.
    CLEAR lw_action_line.
    lw_action_line-id       = 'BASIC_SUBMIT'.
    lw_action_line-visible  = cl_wd_uielement=>e_visible-visible.
    lw_action_line-text     = text-014.
    lw_action_line-enabled  = abap_true.
    APPEND lw_action_line TO et_action_definition.
    CLEAR lw_action_line.
    lw_action_line-id       = 'BASIC_REJECT'.
    lw_action_line-visible  = cl_wd_uielement=>e_visible-visible.
    lw_action_line-text     = text-015.
    lw_action_line-enabled  = abap_true.
    APPEND lw_action_line TO et_action_definition.
    CLEAR lw_action_line.
    lw_action_line-id       = 'BASIC_APPROVAL'.
    lw_action_line-visible  = cl_wd_uielement=>e_visible-visible.
    lw_action_line-text     = text-016.
    lw_action_line-enabled  = abap_true.
    APPEND lw_action_line TO et_action_definition.
    CLEAR lw_action_line.
    lw_action_line-id       = 'BASIC_UPDATE'.
    lw_action_line-visible  = cl_wd_uielement=>e_visible-visible.
    lw_action_line-text     = text-017.
    lw_action_line-enabled  = abap_true.
    APPEND lw_action_line TO et_action_definition.

  ENDMETHOD.


  METHOD if_fpm_guibb_form~process_event.
    DATA: lv_alternum TYPE ztvenalter-alternum,
          lw_message  TYPE fpmgb_s_t100_message.

    CASE io_event->mv_event_id.
*1 提交
      WHEN 'BASIC_SUBMIT'.
        CALL METHOD me->m_handle_submit
          IMPORTING
            et_messages = et_messages.

*2 拒绝
      WHEN 'BASIC_REJECT'.
        CALL METHOD me->m_handle_reject
          IMPORTING
            et_messages = et_messages.

*3 发起审批
      WHEN 'BASIC_APPROVAL'.
        CALL METHOD me->m_handle_approval
          IMPORTING
            et_messages = et_messages.

*4 更新
      WHEN 'BASIC_UPDATE'.
        CALL METHOD me->m_handle_update
          IMPORTING
            et_messages = et_messages.

*5 修改操作
      WHEN 'CHANGE_OPERATION'.
        SELECT COUNT(*)
          FROM ztvenstatus
         WHERE partnernum = gs_form-partnernum
           AND venrank = '04'.   "退出
        IF sy-subrc = 0.
          IF gs_form-operation <> '04'.
            CLEAR gs_form-operation.
            CLEAR:lw_message.
            lw_message-msgid = 'ZSRM'.
            lw_message-msgno = '133'.
            lw_message-severity = 'E'.
            APPEND lw_message TO et_messages.
            RETURN.
          ENDIF.
        ENDIF.

        SELECT SINGLE alternum INTO lv_alternum
          FROM ztvenalter
         WHERE altertype = '01'
           AND formalcode = gs_form-partner
           AND operation = gs_form-operation
           AND ( status = '01' OR
                 status = '02' OR
                 status = '03' OR
                 status = '04' OR
                 status = '05' OR
                 status = '06' OR
                 status = '08' )
           AND alternum <> gs_form-alternum.
        IF sy-subrc = 0.
          CLEAR gs_form-operation.
          CLEAR:lw_message.
          lw_message-msgid = 'ZSRM'.
          lw_message-msgno = '125'.
          lw_message-severity = 'E'.
          lw_message-parameter_1 = lv_alternum.
          APPEND lw_message TO et_messages.
          RETURN.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.


  METHOD if_fpm_guibb_list~check_config.
  ENDMETHOD.


  METHOD if_fpm_guibb_list~flush.
    FIELD-SYMBOLS: <fs_value> TYPE any.
    ASSIGN it_data->* TO <fs_value>.
    gt_list = <fs_value>.

  ENDMETHOD.


  METHOD if_fpm_guibb_list~get_data.
    DATA: lt_ztfile      TYPE STANDARD TABLE OF ztfile,
          lw_ztfile      TYPE ztfile,
          lw_list        TYPE ty_list,
          lw_field_usage TYPE fpmgb_s_fieldusage.

    CASE iv_eventid->mv_event_id.
*1 数据初始化
      WHEN cl_fpm_event=>gc_event_open_dialog_box.
        IF gv_alternum IS INITIAL.
          SELECT
            filetype
            ztvenfile1~zfnumb AS exzfnumb
            startdate AS exstartdate
            deadline AS exdeadline
            zfname AS exzfname
            INTO CORRESPONDING FIELDS OF TABLE gt_list
            FROM ztvenfile1
            JOIN ztfile
              ON ztvenfile1~zfnumb = ztfile~zfnumb
           WHERE partnernum = gs_form-partnernum
             AND ( filetype = 'BL' OR
                   filetype = 'TC' OR
                   filetype = 'BA' ).

        ELSE.
          SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_list
            FROM ztvenfile3
           WHERE alternum = gv_alternum.

          CLEAR: lt_ztfile.
          SELECT * APPENDING CORRESPONDING FIELDS OF TABLE lt_ztfile
            FROM ztfile
             FOR ALL ENTRIES IN gt_list
           WHERE zfnumb = gt_list-exzfnumb
             AND zfnumb <> ''.
          SELECT * APPENDING CORRESPONDING FIELDS OF TABLE lt_ztfile
            FROM ztfile
             FOR ALL ENTRIES IN gt_list
           WHERE zfnumb = gt_list-zfnumb
             AND zfnumb <> ''.
          SORT lt_ztfile BY zfnumb.
          LOOP AT gt_list INTO lw_list.
            READ TABLE lt_ztfile INTO lw_ztfile
              WITH KEY zfnumb = lw_list-zfnumb BINARY SEARCH.
            IF sy-subrc = 0.
              lw_list-zfname = lw_ztfile-zfname.
            ENDIF.
            READ TABLE lt_ztfile INTO lw_ztfile
              WITH KEY zfnumb = lw_list-exzfnumb BINARY SEARCH.
            IF sy-subrc = 0.
              lw_list-exzfname = lw_ztfile-zfname.
            ENDIF.
            MODIFY gt_list FROM lw_list.
          ENDLOOP.
        ENDIF.

        LOOP AT gt_list INTO lw_list.
          IF lw_list-if_change = ''.
            lw_list-change_ref = 'X'.
          ENDIF.
          lw_list-enabled_ref = lw_list-if_change.
          lw_list-upload = text-035.
          MODIFY gt_list FROM lw_list.
        ENDLOOP.

        IF gs_form-operation = '02' OR gs_form-operation IS INITIAL.
          SELECT COUNT(*) FROM ztvenalter WHERE alternum = gv_alternum AND status <> '01' AND status <> '03' AND status <> '05'.
          IF sy-subrc = 0.
            me->m_set_visible_by_operation( EXPORTING read_only = abap_true CHANGING ct_list_fieldusage = ct_field_usage ).
          ELSE.
            me->m_set_visible_by_operation( EXPORTING read_only = abap_false CHANGING ct_list_fieldusage = ct_field_usage ).
          ENDIF.

        ELSE.
          me->m_set_visible_by_operation( EXPORTING read_only = abap_true CHANGING ct_list_fieldusage = ct_field_usage ).
        ENDIF.
*  1.2.3 根据状态设置可编辑
        me->m_set_visible_by_status( EXPORTING alternum = gv_alternum CHANGING ct_list_fieldusage = ct_field_usage ).

        ev_field_usage_changed = abap_true.


*2 关闭上传文件对话框
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

      WHEN 'CHANGE_OPERATION'.
        IF gs_form-operation = '02'.
          me->m_set_visible_by_operation( EXPORTING read_only = abap_false CHANGING ct_list_fieldusage = ct_field_usage ).

        ELSE.
          me->m_set_visible_by_operation( EXPORTING read_only = abap_true CHANGING ct_list_fieldusage = ct_field_usage ).
        ENDIF.
        ev_field_usage_changed = abap_true.


*3 提交
      WHEN 'BASIC_SUBMIT'.
        me->m_set_visible_by_status( EXPORTING alternum = gv_alternum CHANGING ct_form_fieldusage = ct_field_usage ct_action = ct_action_usage ).
        ev_action_usage_changed = abap_true.
        ev_field_usage_changed = abap_true.
*4 拒绝
      WHEN 'BASIC_REJECT'.
        me->m_set_visible_by_status( EXPORTING alternum = gv_alternum CHANGING ct_form_fieldusage = ct_field_usage ct_action = ct_action_usage ).
        ev_action_usage_changed = abap_true.
        ev_field_usage_changed = abap_true.
*5 更新
      WHEN 'BASIC_UPDATE'.
        me->m_set_visible_by_status( EXPORTING alternum = gv_alternum CHANGING ct_form_fieldusage = ct_field_usage ct_action = ct_action_usage ).
        ev_action_usage_changed = abap_true.
        ev_field_usage_changed = abap_true.
*6 发起审批
      WHEN 'BASIC_APPROVAL'.
        me->m_set_visible_by_status( EXPORTING alternum = gv_alternum CHANGING ct_form_fieldusage = ct_field_usage ct_action = ct_action_usage ).
        ev_action_usage_changed = abap_true.
        ev_field_usage_changed = abap_true.
      WHEN OTHERS.
    ENDCASE.

    ct_data = gt_list.
    ev_data_changed = abap_true.
  ENDMETHOD.


  METHOD if_fpm_guibb_list~get_default_config.
  ENDMETHOD.


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
        WHEN 'FILETYPE'.
          lw_field_descr-header_label = text-029.
          lw_field_descr-read_only = abap_true.
        WHEN 'EXZFNAME'.
          lw_field_descr-header_label = text-030.
        WHEN 'EXSTARTDATE'.
          lw_field_descr-header_label = text-031.
        WHEN 'EXDEADLINE'.
          lw_field_descr-header_label = text-032.
        WHEN 'IF_CHANGE'.
          lw_field_descr-header_label = text-033.
        WHEN 'ZFNAME'.
          lw_field_descr-header_label = text-034.
          lw_field_descr-mandatory = abap_true.
        WHEN 'UPLOAD'.
          lw_field_descr-header_label = text-035.
          lw_field_descr-enabled_ref = 'ENABLED_REF'.
        WHEN 'STARTDATE'.
          lw_field_descr-header_label = text-036.
        WHEN 'DEADLINE'.
          lw_field_descr-header_label = text-037.
        WHEN OTHERS.
      ENDCASE.

      IF lw_field_descr-name = 'STARTDATE' OR
         lw_field_descr-name = 'DEADLINE'.
        lw_field_descr-read_only_ref = 'CHANGE_REF'.
        lw_field_descr-mandatory = abap_true.
      ENDIF.
      APPEND lw_field_descr TO et_field_description.
      CLEAR lw_field_descr.
    ENDLOOP.


*-----------------------------------------------------------------------
* 设置按钮
*-----------------------------------------------------------------------
    CLEAR lw_action_line.
    lw_action_line-id       = 'BASIC_IF_CHANGE'.
    lw_action_line-visible  = cl_wd_uielement=>e_visible-visible.
    lw_action_line-enabled  = abap_true.
    lw_action_line-action_type  = 1.
    APPEND lw_action_line TO et_action_definition.
    CLEAR lw_action_line.
    lw_action_line-id       = 'BASIC_DOWNLOAD_EXFILE'.
    lw_action_line-visible  = cl_wd_uielement=>e_visible-visible.
    lw_action_line-enabled  = abap_true.
    lw_action_line-action_type  = 1.
    APPEND lw_action_line TO et_action_definition.
    CLEAR lw_action_line.
    lw_action_line-id       = 'BASIC_DOWNLOAD_FILE'.
    lw_action_line-visible  = cl_wd_uielement=>e_visible-visible.
    lw_action_line-enabled  = abap_true.
    lw_action_line-action_type  = 1.
    APPEND lw_action_line TO et_action_definition.
    CLEAR lw_action_line.
    lw_action_line-id       = 'BASIC_UPLOAD_FILE'.
    lw_action_line-visible  = cl_wd_uielement=>e_visible-visible.
    lw_action_line-enabled  = abap_true.
    lw_action_line-action_type  = 1.
    APPEND lw_action_line TO et_action_definition.

  ENDMETHOD.


  METHOD if_fpm_guibb_list~process_event.
    DATA: lw_list TYPE ty_list.
    DATA: lw_param TYPE fpm_s_dialog_box_properties.

    CASE io_event->mv_event_id.
*1 下载原附件
      WHEN 'BASIC_DOWNLOAD_EXFILE'.
        me->m_download_file( EXPORTING iv_zfnumb = gt_list[ iv_event_index ]-exzfnumb ).
*2 下载新附件
      WHEN 'BASIC_DOWNLOAD_FILE'.
        me->m_download_file( EXPORTING iv_zfnumb    = gt_list[ iv_event_index ]-zfnumb
                                       iv_zfile     = gt_list[ iv_event_index ]-zfile
                                       iv_filename  = gt_list[ iv_event_index ]-zfname
                                       iv_mine_type = gt_list[ iv_event_index ]-zftype ).

*3 选择是否变更
      WHEN 'BASIC_IF_CHANGE'.
        READ TABLE gt_list INTO lw_list INDEX iv_event_index.
        IF sy-subrc = 0.
          IF lw_list-if_change = 'X'.
            lw_list-change_ref = abap_false.
            lw_list-enabled_ref = abap_true.
          ELSE.
            lw_list-change_ref = abap_true.
            lw_list-enabled_ref = abap_false.
          ENDIF.
          MODIFY gt_list FROM lw_list INDEX iv_event_index.
        ENDIF.

*上传附件
      WHEN 'BASIC_UPLOAD_FILE'.
        lw_param-height = '200'.
        lw_param-width = '600'.
        lw_param-title = text-038.
        y_cl_wd_tool=>open_dialog( iv_param = lw_param iv_page = 'UPLOAD_FILE' ).
        gv_event_index = iv_event_index.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.


  METHOD if_fpm_guibb~get_parameter_list.
  ENDMETHOD.


  METHOD if_fpm_guibb~initialize.


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


  METHOD m_handle_approval.
    DATA: lw_message     TYPE fpmgb_s_t100_message,
          lw_change_data TYPE zzmt_finance_view,
          lv_mtype       TYPE bapi_mtype,
          lv_message     TYPE bapi_msg,
          lw_list        TYPE ty_list,
          lv_zfnumb      TYPE ztfile-zfnumb,
          lv_zfname      TYPE ztfile-zfname,
          lv_url         TYPE string,
          lw_ztvenalter  TYPE ztvenalter,
          lv_oa_url      TYPE zoa2url.

*1 数据校验
    SELECT COUNT(*) FROM ztvenalter WHERE alternum = gv_alternum AND status = '01'.
    IF sy-subrc <> 0.
      CLEAR:lw_message.
      lw_message-msgid = 'ZSRM'.
      lw_message-msgno = '122'.
      lw_message-severity = 'E'.
      APPEND lw_message TO et_messages.
      RETURN.
    ENDIF.

*2 调用接口
    CLEAR lw_change_data.
*  申请人/申请人账号
    lw_change_data-user = sy-uname.
    SELECT SINGLE name_text INTO lw_change_data-uname
      FROM usr21
      JOIN adrp
        ON usr21~persnumber = adrp~persnumber
     WHERE usr21~bname = sy-uname.
*  操作类型
    lw_change_data-action = gs_form-operation.


    SELECT SINGLE *
      INTO CORRESPONDING FIELDS OF lw_change_data
      FROM ztvenstatus
     WHERE partnernum = gs_form-partnernum.

    SELECT SINGLE *
      INTO CORRESPONDING FIELDS OF lw_change_data
      FROM ztbasic1
     WHERE partnernum = gs_form-partnernum.

    SELECT SINGLE *
      INTO CORRESPONDING FIELDS OF lw_change_data
      FROM ztvencreate
     WHERE partnernum = gs_form-partnernum.

    SELECT SINGLE *
      INTO CORRESPONDING FIELDS OF lw_change_data
      FROM ztvencoth
     WHERE partnernum = gs_form-partnernum
       AND zposition = '03'.
    MOVE-CORRESPONDING gs_form TO lw_change_data.

*    IF gs_form-operation = '03'.   "冻结
*      lw_change_data-venrank = '04'.
*    ELSEIF gs_form-operation = '04'. "解冻
*      lw_change_data-venrank = '02'.
*    ENDIF.

    IF gs_form-operation = '05'.
      lw_change_data-venrank = gs_form-venrank.
    ENDIF.

    lw_change_data-partnernum = gs_form-alternum.

    LOOP AT gt_list INTO lw_list.
      CLEAR: lv_zfnumb,lv_zfname,lv_url.
      IF lw_list-if_change = 'X'.
        lv_zfnumb = lw_list-zfnumb.
        lv_zfname = lw_list-zfname.
      ELSE.
        lv_zfnumb = lw_list-exzfnumb.
        lv_zfname = lw_list-exzfname.
      ENDIF.

      CALL FUNCTION 'ZSCMS_SPLICE_FILE_URL'
        EXPORTING
          i_crep_id = 'Z1'
          i_zfnumb  = lv_zfnumb
        IMPORTING
          e_url     = lv_url.

      IF lw_list-filetype = 'BL'.
        lw_change_data-filename2 = lv_zfname.
        lw_change_data-fileurl2 = lv_url.

      ELSEIF lw_list-filetype = 'TC'.
        lw_change_data-filename3 = lv_zfname.
        lw_change_data-fileurl3 = lv_url.

      ELSEIF lw_list-filetype = 'BA'.
        lw_change_data-filename4 = lv_zfname.
        lw_change_data-fileurl4 = lv_url.
      ENDIF.
    ENDLOOP.

    IF gs_form-exname1 <> gs_form-name1.
      lw_change_data-changeflag = lw_change_data-changeflag && '/' && 'NAME1'.
    ENDIF.
    IF gs_form-exname2 <> gs_form-name2.
      lw_change_data-changeflag = lw_change_data-changeflag && '/' && 'NAME2'.
    ENDIF.
    IF gs_form-exsimplename <> gs_form-simplename.
      lw_change_data-changeflag = lw_change_data-changeflag && '/' && 'SIMPLENAME'.
    ENDIF.
    IF gs_form-exvenaddress <> gs_form-venaddress.
      lw_change_data-changeflag = lw_change_data-changeflag && '/' && 'VENADDRESS'.
    ENDIF.
    IF gs_form-exbnslicense <> gs_form-bnslicense.
      lw_change_data-changeflag = lw_change_data-changeflag && '/' && 'BNSLICENSE'.
    ENDIF.
    IF gs_form-exbanknation <> gs_form-banknation.
      lw_change_data-changeflag = lw_change_data-changeflag && '/' && 'BANKNATION'.
    ENDIF.
    IF gs_form-exbankname <> gs_form-bankname.
      lw_change_data-changeflag = lw_change_data-changeflag && '/' && 'BANKNAME'.
    ENDIF.
    IF gs_form-exbankaccount <> gs_form-bankaccount.
      lw_change_data-changeflag = lw_change_data-changeflag && '/' && 'BANKACCOUNT'.
    ENDIF.
    IF gs_form-exbankowner <> gs_form-bankowner.
      lw_change_data-changeflag = lw_change_data-changeflag && '/' && 'BANKOWNER'.
    ENDIF.
    IF gs_form-exbankkey <> gs_form-bankkey.
      lw_change_data-changeflag = lw_change_data-changeflag && '/' && 'BANKKEY'.
    ENDIF.
    IF gs_form-exbankkey1 <> gs_form-bankkey1.
      lw_change_data-changeflag = lw_change_data-changeflag && '/' && 'BANKKEY1'.
    ENDIF.
    IF gs_form-excurrency <> gs_form-currency.
      lw_change_data-changeflag = lw_change_data-changeflag && '/' && 'CURRENCY'.
    ENDIF.
    IF gs_form-expaymentterm <> gs_form-paymentterm.
      lw_change_data-changeflag = lw_change_data-changeflag && '/' && 'PAYMENTTERM'.
    ENDIF.
    IF gs_form-exincoterms <> gs_form-incoterms.
      lw_change_data-changeflag = lw_change_data-changeflag && '/' && 'INCOTERMS'.
    ENDIF.
    IF gs_form-exincotermadd <> gs_form-incotermadd.
      lw_change_data-changeflag = lw_change_data-changeflag && '/' && 'INCOTERMADD'.
    ENDIF.
    IF gs_form-exmeprf <> gs_form-meprf.
      lw_change_data-changeflag = lw_change_data-changeflag && '/' && 'MEPRF'.
    ENDIF.

    CLEAR: lv_mtype,lv_message,lv_oa_url.
    CALL FUNCTION 'Z_SRM_OA_CHANGE_VENDOR'
      EXPORTING
        i_mt_change_vendor = lw_change_data
      IMPORTING
        e_mtype            = lv_mtype
        e_message          = lv_message
        e_url              = lv_oa_url.
    IF lv_mtype <> 'S'.
      CLEAR:lw_message.
      lw_message-msgid = 'ZSRM'.
      lw_message-msgno = '021'.
      lw_message-severity = 'E'.
      lw_message-parameter_1 = lv_message.
      APPEND lw_message TO et_messages.
      RETURN.
    ENDIF.

*3 更新数据
* 3.1 更新ZTVENALTER
    SELECT SINGLE * INTO CORRESPONDING FIELDS OF lw_ztvenalter
      FROM ztvenalter
     WHERE alternum = gv_alternum.
    lw_ztvenalter-status = '04'.
    lw_ztvenalter-url = lv_oa_url.
    MODIFY ztvenalter FROM lw_ztvenalter.
* 3.2 更新前台数据
    gs_form-status = '04'.

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

*1 数据校验
    SELECT COUNT(*)
      FROM ztvenalter
     WHERE alternum = gv_alternum
       AND ( status = '01' OR
           status = '02' OR
           status = '03' OR
           status = '05' ).
    IF sy-subrc <> 0.
      CLEAR:lw_message.
      lw_message-msgid = 'ZSRM'.
      lw_message-msgno = '120'.
      lw_message-severity = 'E'.
      APPEND lw_message TO et_messages.
      RETURN.
    ENDIF.

*2 更新数据库表
    SELECT SINGLE * INTO CORRESPONDING FIELDS OF lw_ztvenalter
      FROM ztvenalter
     WHERE alternum = gv_alternum.
    lw_ztvenalter-status = '03'.
    lw_ztvenalter-puruser = sy-uname.
    lw_ztvenalter-purdate = sy-datum.
    MODIFY ztvenalter FROM lw_ztvenalter.

*3 更新前台数据
    gs_form-status = '03'.

*4 输出消息
    CLEAR:lw_message.
    lw_message-msgid = 'ZSRM'.
    lw_message-msgno = '041'.
    lw_message-severity = 'S'.
    APPEND lw_message TO et_messages.
  ENDMETHOD.


  METHOD m_handle_submit.
    DATA: lw_message       TYPE fpmgb_s_t100_message,
          lw_ztvenalter    TYPE ztvenalter,
          lw_ztvenchgbasic TYPE ztvenchgbasic,
          lw_list          TYPE ty_list,
          lt_ztvenfile3    TYPE STANDARD TABLE OF ztvenfile3.
    DATA: lv_objid       TYPE sapb-sapobjid,
          lv_xstring     TYPE xstring,
          lv_filename255 TYPE toaat-filename,
          lw_ztfile      TYPE ztfile,
          lt_ztfile      TYPE STANDARD TABLE OF ztfile.

*1 数据校验
*  该申请单状态不允许提交！
    IF gv_alternum IS NOT INITIAL.
      SELECT COUNT(*)
        FROM ztvenalter
       WHERE alternum = gv_alternum
         AND ( status = '01' OR
               status = '02' OR
               status = '03' OR
               status = '05' ).
      IF sy-subrc <> 0.
        CLEAR:lw_message.
        lw_message-msgid = 'ZSRM'.
        lw_message-msgno = '118'.
        lw_message-severity = 'E'.
        APPEND lw_message TO et_messages.
        RETURN.
      ENDIF.
    ENDIF.
    LOOP AT gt_list INTO lw_list WHERE if_change = 'X' AND zfname IS INITIAL.
      CLEAR:lw_message.
      lw_message-msgid = 'ZSRM'.
      lw_message-msgno = '017'.
      lw_message-severity = 'E'.
      APPEND lw_message TO et_messages.
      RETURN.
    ENDLOOP.
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

*  数据无变更，不允许提交
    IF ( gs_form-operation = '02' AND
         ( gs_form-name1 = gs_form-exname1 AND
           gs_form-name2 = gs_form-exname2 AND
           gs_form-simplename = gs_form-exsimplename AND
           gs_form-venaddress = gs_form-exvenaddress AND
           gs_form-bnslicense = gs_form-exbnslicense AND
           gs_form-banknation = gs_form-exbanknation AND
           gs_form-bankname = gs_form-exbankname AND
           gs_form-bankaccount = gs_form-exbankaccount AND
           gs_form-bankowner = gs_form-exbankowner AND
           gs_form-bankkey = gs_form-exbankkey AND
           gs_form-bankkey1 = gs_form-exbankkey1 AND
           gs_form-currency = gs_form-excurrency AND
           gs_form-paymentterm = gs_form-expaymentterm AND
           gs_form-incoterms = gs_form-exincoterms AND
           gs_form-incotermadd = gs_form-exincotermadd AND
           gs_form-meprf = gs_form-exmeprf ) ) OR
        ( gs_form-operation = '05' AND gs_form-exvenrank = gs_form-venrank ).

      READ TABLE gt_list TRANSPORTING NO FIELDS
        WITH KEY if_change = 'X'.
      IF sy-subrc <> 0.
        CLEAR:lw_message.
        lw_message-msgid = 'ZSRM'.
        lw_message-msgno = '119'.
        lw_message-severity = 'E'.
        APPEND lw_message TO et_messages.
        RETURN.
      ENDIF.
    ENDIF.

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
* 2.2 更新ZTVENALTER
    SELECT SINGLE * INTO CORRESPONDING FIELDS OF lw_ztvenalter
      FROM ztvenalter
     WHERE alternum = gv_alternum.
    IF sy-subrc <> 0.
      lw_ztvenalter-alternum = gv_alternum.
      lw_ztvenalter-operation = gs_form-operation.
      lw_ztvenalter-altertype = '01'.
      lw_ztvenalter-applydate = sy-datum.
      lw_ztvenalter-applytime = sy-uzeit.
      lw_ztvenalter-applyuser = sy-uname.
      lw_ztvenalter-status = '01'.
      lw_ztvenalter-partnernum = gs_form-partnernum.
      lw_ztvenalter-formalcode = gs_form-partner.
    ELSE.
      lw_ztvenalter-status = '01'.
      lw_ztvenalter-operation = gs_form-operation.
      lw_ztvenalter-applydate = sy-datum.
      lw_ztvenalter-applytime = sy-uzeit.
      lw_ztvenalter-applyuser = sy-uname.
    ENDIF.
    MODIFY ztvenalter FROM lw_ztvenalter.
* 2.2 更新ZTVANCHGBASIC
    SELECT SINGLE * INTO CORRESPONDING FIELDS OF lw_ztvenchgbasic
      FROM ztvenchgbasic
     WHERE alternum = gv_alternum.
    MOVE-CORRESPONDING gs_form TO lw_ztvenchgbasic.
    MODIFY ztvenchgbasic FROM lw_ztvenchgbasic.
*2.3 附件上传服务器
    LOOP AT gt_list INTO lw_list WHERE change_file = 'X'.
*  2.3.1 删除旧附件
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
*  2.3.2 上传新附件
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
*  2.3.3 更新ZTFILE
      MOVE-CORRESPONDING lw_list TO lw_ztfile.
      APPEND lw_ztfile TO lt_ztfile.
    ENDLOOP.
    MODIFY ztfile FROM TABLE lt_ztfile.

* 2.3 更新ZTVENFILE3
    DELETE FROM ztvenfile3 WHERE alternum = gv_alternum.
    MOVE-CORRESPONDING gt_list TO lt_ztvenfile3.
    MODIFY ztvenfile3 FROM TABLE lt_ztvenfile3.

* 2.4 更新前台数据
    gs_form-status = '01'.


*3 输出消息
    CLEAR:lw_message.
    lw_message-msgid = 'ZSRM'.
    lw_message-msgno = '041'.
    lw_message-severity = 'S'.
    APPEND lw_message TO et_messages.


  ENDMETHOD.


  METHOD m_handle_update.
    DATA: lw_message     TYPE fpmgb_s_t100_message,
          lw_ztbasic1    TYPE ztbasic1,
          lt_ztvenfi     TYPE STANDARD TABLE OF ztvenfi,
          lw_ztvenfi     TYPE ztvenfi,
          lt_list        TYPE STANDARD TABLE OF ty_list,
          lw_list        TYPE ty_list,
          lt_ztvenfile1  TYPE STANDARD TABLE OF ztvenfile1,
          lw_ztvenfile1  TYPE ztvenfile1,
          lw_ztvenalter  TYPE ztvenalter,
          lw_ztvenstatus TYPE ztvenstatus,
          lw_ztvencreate TYPE ztvencreate,
          error_flag     TYPE char1.
    DATA: lw_header  TYPE zzvendor_change_header,
          lt_contact TYPE ztvendor_create_contact,
          lw_contact TYPE zzvendor_create_contact,
          lv_msgty   TYPE msgty,
          lv_message TYPE bapi_msg.

*1 数据校验
    SELECT     COUNT(*)
          FROM ztvenalter
         WHERE alternum = gv_alternum
           AND ( status = '06' OR
                 status = '08' ).
    IF sy-subrc <> 0.
      CLEAR:lw_message.
      lw_message-msgid = 'ZSRM'.
      lw_message-msgno = '121'.
      lw_message-severity = 'E'.
      APPEND lw_message TO et_messages.
      RETURN.
    ENDIF.

*2 更新数据
    IF gs_form-operation = '02'.

* 2.1 修改
*  2.1.1 调用接口更新ecc
      CLEAR lw_header.
      lw_header-lifnr = gs_form-partner.
      lw_header-ekorg = gs_form-ekorg.
      lw_header-name1 = gs_form-name1.
      lw_header-name2 = gs_form-name2.
      lw_header-sort1 = gs_form-simplename.
      lw_header-street = gs_form-venaddress.
      lw_header-stceg = gs_form-bnslicense.
      lw_header-bankl = gs_form-bankkey.
      lw_header-banks = gs_form-banknation.
      lw_header-bankn = gs_form-bankaccount.
      lw_header-koinh = gs_form-bankowner.
      lw_header-waers = gs_form-currency.
      lw_header-zterm = gs_form-paymentterm.
      lw_header-inco1 = gs_form-incoterms.
      lw_header-inco2 = gs_form-incotermadd.
      lw_header-meprf = gs_form-meprf.

      CALL FUNCTION 'Z_SRM_ECC_CHANGE_VENDOR'
        EXPORTING
          i_chgtype = '03'
          i_header  = lw_header
        IMPORTING
          e_msgty   = lv_msgty
          e_message = lv_message.
      IF lv_msgty <> 'S'.
        gs_form-status = '08'.

        CLEAR:lw_message.
        lw_message-msgid = 'ZSRM'.
        lw_message-msgno = '021'.
        lw_message-severity = 'E'.
        lw_message-parameter_1 = lv_message.
        APPEND lw_message TO et_messages.
        RETURN.
      ENDIF.

*  2.1.2 更新ZTBASIC1
      SELECT SINGLE * INTO CORRESPONDING FIELDS OF lw_ztbasic1
        FROM ztbasic1
       WHERE partnernum = gs_form-partnernum.
      MOVE-CORRESPONDING gs_form TO lw_ztbasic1.
      MODIFY ztbasic1 FROM lw_ztbasic1.
*  2.1.3 更新ZTVENFI
      SELECT SINGLE * INTO CORRESPONDING FIELDS OF lw_ztvenfi
        FROM ztvenfi
       WHERE partnernum = gs_form-partnernum.
      MOVE-CORRESPONDING gs_form TO lw_ztvenfi.
      MODIFY ztvenfi FROM lw_ztvenfi.
*  2.1.4 更新ZTVENFILE1
      lt_list = gt_list.
      DELETE lt_list WHERE if_change = ''.
      IF lt_list IS NOT INITIAL.
        SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_ztvenfile1
          FROM ztvenfile1
           FOR ALL ENTRIES IN lt_list
         WHERE filetype = lt_list-filetype
           AND partnernum = gs_form-partnernum.
        LOOP AT lt_ztvenfile1 INTO lw_ztvenfile1.
          MOVE-CORRESPONDING gt_list[ filetype = lw_ztvenfile1-filetype ] TO lw_ztvenfile1.
          MODIFY lt_ztvenfile1 FROM lw_ztvenfile1.
        ENDLOOP.
      ENDIF.
      MODIFY ztvenfile1 FROM TABLE lt_ztvenfile1.
*  2.1.5 更新ZTVENCREATE
      SELECT SINGLE * INTO CORRESPONDING FIELDS OF lw_ztvencreate
        FROM ztvencreate
       WHERE partnernum = gs_form-partnernum.
      lw_ztvencreate-meprf = gs_form-meprf.
      MODIFY ztvencreate FROM lw_ztvencreate.


* 2.2 退出
    ELSEIF gs_form-operation = '03'.
      CLEAR lw_header.
      lw_header-lifnr = gs_form-partner.
      CALL FUNCTION 'Z_SRM_ECC_CHANGE_VENDOR'
        EXPORTING
          i_chgtype = '01'
          i_header  = lw_header
        IMPORTING
          e_msgty   = lv_msgty
          e_message = lv_message.
      IF lv_msgty <> 'S'.
        gs_form-status = '08'.
        error_flag = 'X'.

        CLEAR:lw_message.
        lw_message-msgid = 'ZSRM'.
        lw_message-msgno = '021'.
        lw_message-severity = 'E'.
        lw_message-parameter_1 = lv_message.
        APPEND lw_message TO et_messages.

      ELSE.
        SELECT SINGLE * INTO CORRESPONDING FIELDS OF lw_ztvenstatus
          FROM ztvenstatus
         WHERE partnernum = gs_form-partnernum.
        IF sy-subrc = 0.
          lw_ztvenstatus-venrank = '04'.
          MODIFY ztvenstatus FROM lw_ztvenstatus.
        ENDIF.
      ENDIF.


* 2.3 解冻
    ELSEIF gs_form-operation = '04'.
      CLEAR lw_header.
      lw_header-lifnr = gs_form-partner.
      CALL FUNCTION 'Z_SRM_ECC_CHANGE_VENDOR'
        EXPORTING
          i_chgtype = '02'
          i_header  = lw_header
        IMPORTING
          e_msgty   = lv_msgty
          e_message = lv_message.
      IF lv_msgty <> 'S'.
        gs_form-status = '08'.
        error_flag = 'X'.

        CLEAR:lw_message.
        lw_message-msgid = 'ZSRM'.
        lw_message-msgno = '021'.
        lw_message-severity = 'E'.
        lw_message-parameter_1 = lv_message.
        APPEND lw_message TO et_messages.

      ELSE.
        SELECT SINGLE * INTO CORRESPONDING FIELDS OF lw_ztvenstatus
          FROM ztvenstatus
         WHERE partnernum = gs_form-partnernum.
        IF sy-subrc = 0.
          lw_ztvenstatus-venrank = '02'.
          MODIFY ztvenstatus FROM lw_ztvenstatus.
        ENDIF.
      ENDIF.


* 2.4 状态变更
    ELSEIF gs_form-operation = '05'.
*  2.4.1 更新ZTVENSTATUS
      SELECT SINGLE * INTO CORRESPONDING FIELDS OF lw_ztvenstatus
        FROM ztvenstatus
       WHERE partnernum = gs_form-partnernum.
      lw_ztvenstatus-venrank = gs_form-venrank.
      MODIFY ztvenstatus FROM lw_ztvenstatus.
    ENDIF.


* 2.5 更新ZTVENALTER
    SELECT SINGLE * INTO CORRESPONDING FIELDS OF lw_ztvenalter
      FROM ztvenalter
     WHERE alternum = gv_alternum.
    lw_ztvenalter-updateuser = sy-uname.
    lw_ztvenalter-updatedate = sy-datum.
    IF error_flag = ''.
      lw_ztvenalter-status = '07'.
    ELSE.
      lw_ztvenalter-status = '08'.
    ENDIF.
    MODIFY ztvenalter FROM lw_ztvenalter.

*2.6 更新页面状态
    gs_form-status = lw_ztvenalter-status.

*3 输出消息
    CLEAR:lw_message.
    lw_message-msgid = 'ZSRM'.
    lw_message-msgno = '041'.
    lw_message-severity = 'S'.
    APPEND lw_message TO et_messages.
  ENDMETHOD.


  METHOD m_set_visible_by_operation.
    DATA: lw_fieldusage TYPE fpmgb_s_fieldusage,
          lw_list       TYPE ty_list.

    LOOP AT ct_list_fieldusage INTO lw_fieldusage WHERE name <> 'FILETYPE'.
      lw_fieldusage-read_only = read_only.
      MODIFY ct_list_fieldusage FROM lw_fieldusage.
    ENDLOOP.

    LOOP AT ct_form_fieldusage INTO lw_fieldusage
      WHERE name = 'NAME1' OR
            name = 'NAME2' OR
            name = 'SIMPLENAME' OR
            name = 'VENADDRESS' OR
            name = 'BNSLICENSE' OR
            name = 'BANKNATION' OR
            name = 'BANKNAME' OR
            name = 'BANKACCOUNT' OR
            name = 'BANKOWNER' OR
            name = 'BANKKEY' OR
            name = 'BANKKEY1' OR
            name = 'CURRENCY' OR
            name = 'PAYMENTTERM' OR
            name = 'INCOTERMS' OR
            name = 'INCOTERMADD' OR
            name = 'MEPRF' OR
            name = 'EKORG'.
      lw_fieldusage-read_only = read_only.
      MODIFY ct_form_fieldusage FROM lw_fieldusage.
    ENDLOOP.

    IF read_only = 'X'.
      IF gs_form-operation = '02'.
        LOOP AT gt_list INTO lw_list.
          lw_list-change_ref = 'X'.
          lw_list-enabled_ref = abap_false.
          MODIFY gt_list FROM lw_list.
        ENDLOOP.
      ENDIF.
    ELSE.
      IF gs_form-operation = '02'.
        LOOP AT gt_list INTO lw_list WHERE if_change = 'X'.
          lw_list-change_ref = ''.
          lw_list-enabled_ref = abap_true.
          MODIFY gt_list FROM lw_list.
        ENDLOOP.
      ENDIF.
    ENDIF.


*    重置更改内容
    IF gs_form-operation = '03' OR gs_form-operation = '04' OR gs_form-operation = '05'.
      LOOP AT gt_list INTO lw_list.
        lw_list-if_change = ''.
        CLEAR: lw_list-if_change,lw_list-zfname,lw_list-startdate,lw_list-deadline,lw_list-zfile,lw_list-zfnumb.
        lw_list-change_ref = 'X'.
        lw_list-enabled_ref = abap_false.
        MODIFY gt_list FROM lw_list.
      ENDLOOP.

      gs_form-exname1       = gs_form-name1      .
      gs_form-exname2       = gs_form-name2      .
      gs_form-exsimplename  = gs_form-simplename .
      gs_form-exvenaddress  = gs_form-venaddress .
      gs_form-exbnslicense  = gs_form-bnslicense .
      gs_form-exbanknation  = gs_form-banknation .
      gs_form-exbankname    = gs_form-bankname   .
      gs_form-exbankaccount = gs_form-bankaccount.
      gs_form-exbankowner   = gs_form-bankowner  .
      gs_form-exbankkey     = gs_form-bankkey    .
      gs_form-exbankkey1    = gs_form-bankkey1   .
      gs_form-excurrency    = gs_form-currency   .
      gs_form-expaymentterm = gs_form-paymentterm.
      gs_form-exincoterms   = gs_form-incoterms  .
      gs_form-exincotermadd = gs_form-incotermadd.
      gs_form-exmeprf       = gs_form-meprf      .
    ENDIF.
  ENDMETHOD.


METHOD m_set_visible_by_status.
  DATA: lw_fieldusage TYPE fpmgb_s_fieldusage,
        lw_list       TYPE ty_list,
        lw_action     TYPE fpmgb_s_actionusage,
        lv_status     TYPE ztvenalter-status.

  CLEAR lv_status.
  SELECT SINGLE status INTO lv_status
    FROM ztvenalter
   WHERE alternum = alternum.

  LOOP AT ct_list_fieldusage INTO lw_fieldusage WHERE name <> 'FILETYPE'.
    IF ( lv_status = '01' OR lv_status = '03' OR lv_status = '' ) AND ( gs_form-operation = '02' OR gs_form-operation = '' ).
      lw_fieldusage-read_only = abap_false.
    ELSE.
      lw_fieldusage-read_only = abap_true.
    ENDIF.
    MODIFY ct_list_fieldusage FROM lw_fieldusage.
  ENDLOOP.

  LOOP AT ct_form_fieldusage INTO lw_fieldusage
    WHERE name = 'NAME1' OR
          name = 'NAME2' OR
          name = 'SIMPLENAME' OR
          name = 'VENADDRESS' OR
          name = 'BNSLICENSE' OR
          name = 'BANKNATION' OR
          name = 'BANKNAME' OR
          name = 'BANKACCOUNT' OR
          name = 'BANKOWNER' OR
          name = 'BANKKEY' OR
          name = 'BANKKEY1' OR
          name = 'CURRENCY' OR
          name = 'PAYMENTTERM' OR
          name = 'INCOTERMS' OR
          name = 'INCOTERMADD' OR
          name = 'MEPRF' OR
*          name = 'OPERATION' OR
          name = 'VENRANK' OR
          name = 'EKORG'.
    IF ( lv_status = '01' OR lv_status = '03' OR lv_status = '05' OR lv_status = '' ) AND ( gs_form-operation = '02' OR gs_form-operation = '' ).
      lw_fieldusage-read_only = abap_false.
    ELSE.
      lw_fieldusage-read_only = abap_true.
    ENDIF.
    MODIFY ct_form_fieldusage FROM lw_fieldusage.
  ENDLOOP.

  TRY .
      IF gv_is_partner = 'X'.
        ct_form_fieldusage[ name = 'OPERATION' ]-read_only = abap_true.
      ELSE.
        IF gs_form-alternum = '########'.
          ct_form_fieldusage[ name = 'OPERATION' ]-read_only = abap_false.
        ELSE.
          ct_form_fieldusage[ name = 'OPERATION' ]-read_only = abap_true.
        ENDIF.
      ENDIF.
    CATCH cx_sy_itab_line_not_found.

  ENDTRY.

  IF lv_status <> '01' AND lv_status <> '03' AND lv_status <> '05' AND lv_status <> '' .
    LOOP AT gt_list INTO lw_list.
      lw_list-change_ref = 'X'.
      lw_list-enabled_ref = abap_false.
      MODIFY gt_list FROM lw_list.
    ENDLOOP.
  ELSE.
    LOOP AT gt_list INTO lw_list WHERE if_change = 'X'.
      lw_list-change_ref = ''.
      lw_list-enabled_ref = abap_true.
      MODIFY gt_list FROM lw_list.
    ENDLOOP.
  ENDIF.


  TRY .

      IF lv_status = ''.
        ct_action[ id = 'BASIC_SUBMIT' ]-enabled = abap_true.
        ct_action[ id = 'BASIC_REJECT' ]-enabled = abap_false.
        ct_action[ id = 'BASIC_UPDATE' ]-enabled = abap_false.
        ct_action[ id = 'BASIC_APPROVAL' ]-enabled = abap_false.
      ELSEIF lv_status = '01'.     "已申请
        ct_action[ id = 'BASIC_SUBMIT' ]-enabled = abap_true.
        ct_action[ id = 'BASIC_REJECT' ]-enabled = abap_true.
        ct_action[ id = 'BASIC_UPDATE' ]-enabled = abap_false.
        ct_action[ id = 'BASIC_APPROVAL' ]-enabled = abap_true.
      ELSEIF lv_status = '03'. "采购员拒绝
        ct_action[ id = 'BASIC_SUBMIT' ]-enabled = abap_true.
        ct_action[ id = 'BASIC_REJECT' ]-enabled = abap_false.
        ct_action[ id = 'BASIC_UPDATE' ]-enabled = abap_false.
        ct_action[ id = 'BASIC_APPROVAL' ]-enabled = abap_false.
      ELSEIF lv_status = '04'. "内部审批中
        ct_action[ id = 'BASIC_SUBMIT' ]-enabled = abap_false.
        ct_action[ id = 'BASIC_REJECT' ]-enabled = abap_false.
        ct_action[ id = 'BASIC_UPDATE' ]-enabled = abap_false.
        ct_action[ id = 'BASIC_APPROVAL' ]-enabled = abap_false.
      ELSEIF lv_status = '05'. "内部审批拒绝
        ct_action[ id = 'BASIC_SUBMIT' ]-enabled = abap_true.
        ct_action[ id = 'BASIC_REJECT' ]-enabled = abap_false.
        ct_action[ id = 'BASIC_UPDATE' ]-enabled = abap_false.
        ct_action[ id = 'BASIC_APPROVAL' ]-enabled = abap_false.
      ELSEIF lv_status = '06'. "申请通过
        ct_action[ id = 'BASIC_SUBMIT' ]-enabled = abap_false.
        ct_action[ id = 'BASIC_REJECT' ]-enabled = abap_false.
        ct_action[ id = 'BASIC_UPDATE' ]-enabled = abap_true.
        ct_action[ id = 'BASIC_APPROVAL' ]-enabled = abap_false.
      ELSEIF lv_status = '07'. "更新成功
        ct_action[ id = 'BASIC_SUBMIT' ]-enabled = abap_false.
        ct_action[ id = 'BASIC_REJECT' ]-enabled = abap_false.
        ct_action[ id = 'BASIC_UPDATE' ]-enabled = abap_false.
        ct_action[ id = 'BASIC_APPROVAL' ]-enabled = abap_false.
      ELSEIF lv_status = '08'. "更新失败
        ct_action[ id = 'BASIC_SUBMIT' ]-enabled = abap_false.
        ct_action[ id = 'BASIC_REJECT' ]-enabled = abap_false.
        ct_action[ id = 'BASIC_UPDATE' ]-enabled = abap_true.
        ct_action[ id = 'BASIC_APPROVAL' ]-enabled = abap_false.
      ENDIF.

    CATCH  cx_sy_itab_line_not_found .
  ENDTRY.

ENDMETHOD.
ENDCLASS.
