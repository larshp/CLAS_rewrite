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
  PERFORM run.

FORM run.

  DATA: ls_vseoclass TYPE vseoclass,
        lv_source    TYPE string.


  ls_vseoclass-clsname   = 'ZCL_FOOBAA'.
  ls_vseoclass-version   = '1'.
  ls_vseoclass-langu     = sy-langu.
  ls_vseoclass-descript  = 'foobar'.
  ls_vseoclass-exposure  = '2'.
  ls_vseoclass-state     = '1'.
  ls_vseoclass-clsfinal  = abap_true.
  ls_vseoclass-clsccincl = abap_true.
  ls_vseoclass-fixpt     = abap_true.
  ls_vseoclass-unicode   = abap_true.

  lv_source = |{ lv_source }CLASS zcl_foobaa DEFINITION PUBLIC FINAL CREATE PUBLIC.\n|.
  lv_source = |{ lv_source }  PUBLIC SECTION.\n|.
  lv_source = |{ lv_source }    METHODS foo.\n|.
  lv_source = |{ lv_source }  PROTECTED SECTION.\n|.
  lv_source = |{ lv_source }  PRIVATE SECTION.\n|.
  lv_source = |{ lv_source }ENDCLASS.\n|.
  lv_source = |{ lv_source }CLASS zcl_foobaa IMPLEMENTATION.\n|.
  lv_source = |{ lv_source }  METHOD foo.\n|.
  lv_source = |{ lv_source }  ENDMETHOD.\n|.
  lv_source = |{ lv_source }ENDCLASS.\n|.

  NEW lcl_clas_deser( ls_vseoclass )->run(
    iv_devclass = '$TMP'
    iv_source   = lv_source ).

  WRITE: / 'Done'.

ENDFORM.
