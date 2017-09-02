*&---------------------------------------------------------------------*
*& Report ZCLAS_REWRITE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zclas_rewrite.

INCLUDE zclas_rewrite_utils.
INCLUDE zclas_rewrite_clas.
INCLUDE zclas_rewrite_tests.

START-OF-SELECTION.
  PERFORM run USING 1.
*  PERFORM performance.
* todo, syntax check?
  PERFORM update_package_tree.

FORM performance.

  CONSTANTS: lc_count TYPE i VALUE 10.

  DO lc_count TIMES.
    cl_progress_indicator=>progress_indicate(
        i_text               = |Creating { sy-index }/{ lc_count }|
        i_processed          = sy-index
        i_total              = lc_count
        i_output_immediately = abap_true ).

    PERFORM run USING sy-index.
    COMMIT WORK.
  ENDDO.

ENDFORM.

FORM run USING p_counter TYPE i RAISING cx_oo_clif_component.

  DATA: ls_vseoclass TYPE vseoclass,
        lv_source    TYPE string,
        lv_name      TYPE ddobjname,
        lv_num       TYPE n LENGTH 5.

  lv_num  = p_counter.
  lv_name = |ZCL_FOOBAA{ lv_num }|.

  ls_vseoclass-clsname   = lv_name.
  ls_vseoclass-version   = '1'.
  ls_vseoclass-langu     = sy-langu.
  ls_vseoclass-descript  = 'foobaasar'.
  ls_vseoclass-exposure  = '2'.
  ls_vseoclass-state     = '1'.
  ls_vseoclass-clsfinal  = abap_true.
  ls_vseoclass-clsccincl = abap_true.
  ls_vseoclass-fixpt     = abap_true.
  ls_vseoclass-unicode   = abap_true.

  lv_source = |{ lv_source }CLASS { to_lower( lv_name ) } DEFINITION PUBLIC FINAL CREATE PUBLIC.\n|.
  lv_source = |{ lv_source }  PUBLIC SECTION.\n|.
  lv_source = |{ lv_source }    METHODS foo.\n|.
  lv_source = |{ lv_source }  PROTECTED SECTION.\n|.
  lv_source = |{ lv_source }  PRIVATE SECTION.\n|.
  lv_source = |{ lv_source }ENDCLASS.\n|.
  lv_source = |{ lv_source }CLASS { to_lower( lv_name ) } IMPLEMENTATION.\n|.
  lv_source = |{ lv_source }  METHOD foo.\n|.
  lv_source = |{ lv_source }    WRITE 'foobaaar'.\n|.
  lv_source = |{ lv_source }  ENDMETHOD.\n|.
  lv_source = |{ lv_source }ENDCLASS.\n|.

  NEW lcl_clas_experimental(
    iv_devclass = '$TMP'
    is_class    = ls_vseoclass )->run( lv_source ).

  WRITE: / lv_name.

ENDFORM.

FORM update_package_tree.

  DATA: lv_tree TYPE dirtree-tname.


* update package tree for SE80
  lv_tree = 'EU_$TMP'.
  CALL FUNCTION 'WB_TREE_ACTUALIZE'
    EXPORTING
      tree_name              = lv_tree
      without_crossreference = abap_true
      with_tcode_index       = abap_true.

ENDFORM.
