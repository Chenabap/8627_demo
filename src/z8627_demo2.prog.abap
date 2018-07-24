REPORT z8627_demo2.

*PARAMETERS p_cate TYPE bbp_pdigp-category_id.
**
*SELECT
*  crmd_orderadm_i~parent
*  INTO TABLE @DATA(lt_guid)
*  FROM crmd_orderadm_i
*  JOIN bbp_pdigp
*    ON crmd_orderadm_i~guid = bbp_pdigp~guid
* WHERE crmd_orderadm_i~object_type = 'BUS2200001'
*   AND bbp_pdigp~category_id = @p_cate.
*
*LOOP AT lt_guid  INTO DATA(lw_guid).
*  WRITE lw_guid-parent.
*  WRITE /.
*ENDLOOP.

DATA: lv_str TYPE string VALUE '12345678'.

DATA: lv_char TYPE char20.
TRY .
    lv_char = lv_str+0(20).
  CATCH cx_sy_range_out_of_bounds.

ENDTRY.

WRITE lv_char.
