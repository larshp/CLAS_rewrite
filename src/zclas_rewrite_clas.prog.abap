*&---------------------------------------------------------------------*
*&  Include           ZCLAS_REWRITE_CLAS
*&---------------------------------------------------------------------*

CLASS lcl_clas_deser DEFINITION.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          is_class TYPE vseoclass,
      run
        IMPORTING
          iv_devclass    TYPE devclass
          iv_source      TYPE string
          iv_locals_def  TYPE string OPTIONAL
          iv_locals_imp  TYPE string OPTIONAL
          iv_macros      TYPE string OPTIONAL
          iv_testclasses TYPE string OPTIONAL.

  PRIVATE SECTION.
    DATA: ms_class TYPE vseoclass.

    METHODS:
      init_scanner
        IMPORTING
          iv_source         TYPE string
        RETURNING
          VALUE(ro_scanner) TYPE REF TO cl_oo_source_scanner_class,
      activate,
      scan
        IMPORTING
          iv_source TYPE string,
      create
        IMPORTING
          iv_devclass TYPE devclass.

ENDCLASS.

CLASS lcl_clas_deser IMPLEMENTATION.

  METHOD constructor.

    ms_class = is_class.

  ENDMETHOD.

  METHOD create.

    DATA: ls_class TYPE vseoclass.


    CALL FUNCTION 'SEO_CLASS_CREATE_COMPLETE'
      EXPORTING
        devclass        = iv_devclass
        version         = seoc_version_active
      CHANGING
        class           = ls_class
      EXCEPTIONS
        existing        = 1
        is_interface    = 2
        db_error        = 3
        component_error = 4
        no_access       = 5
        other           = 6
        OTHERS          = 7.
    ASSERT sy-subrc = 0.

  ENDMETHOD.

  METHOD run.

*    create( iv_devclass = iv_devclass
*            is_class    = is_class ).

    scan( iv_source ).

*    DATA(lv_pool_include) = cl_oo_classname_service=>get_interfacepool_name( is_class-clsname ).
*    cl_where_used_list_utilities=>update_index_in_background( p_name = lv_pool_include ).

  ENDMETHOD.

  METHOD init_scanner.

    DATA: lt_source TYPE seop_source_string.

    SPLIT iv_source AT |\n| INTO TABLE lt_source.

    ro_scanner = cl_oo_source_scanner_class=>create_class_scanner(
      clif_name = ms_class-clsname
      source    = lt_source ).
    ro_scanner->scan( ).

  ENDMETHOD.

  METHOD scan.

    DATA(lo_scanner) = init_scanner( iv_source ).


    DATA(lt_source) = lo_scanner->get_public_section_source( ).
    DATA(lv_program) = cl_oo_classname_service=>get_pubsec_name( ms_class-clsname ).

    INSERT REPORT lv_program FROM lt_source.
    ASSERT sy-subrc = 0.

    DATA(lt_methods) = lo_scanner->get_method_implementations( ).
break-point.

* todo
* protected
* private
* methods

  ENDMETHOD.

  METHOD activate.
* todo
  ENDMETHOD.

ENDCLASS.
