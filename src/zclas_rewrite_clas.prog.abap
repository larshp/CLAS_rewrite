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
          iv_testclasses TYPE string OPTIONAL
        RAISING
          cx_oo_clif_component.

  PRIVATE SECTION.
    DATA: ms_class TYPE vseoclass.

    METHODS:
      init_scanner
        IMPORTING
          iv_source         TYPE string
        RETURNING
          VALUE(ro_scanner) TYPE REF TO cl_oo_source_scanner_class,
      check,
      generate_classpool,
      update_meta
        IMPORTING
          iv_exposure TYPE seoexpose
          it_source   TYPE rswsourcet,
      determine_method_include
        IMPORTING
          iv_method         TYPE seocpdname
        RETURNING
          VALUE(rv_program) TYPE programm,
      scan
        IMPORTING
                  iv_source TYPE string
        RAISING   cx_oo_clif_component,
      create
        IMPORTING
          iv_devclass TYPE devclass.

ENDCLASS.

CLASS lcl_clas_deser IMPLEMENTATION.

  METHOD constructor.

    ms_class = is_class.

  ENDMETHOD.

  METHOD create.

    CALL FUNCTION 'SEO_CLASS_CREATE_COMPLETE'
      EXPORTING
        devclass        = iv_devclass
        version         = seoc_version_active
      CHANGING
        class           = ms_class
      EXCEPTIONS
        existing        = 1
        is_interface    = 2
        db_error        = 3
        component_error = 4
        no_access       = 5
        other           = 6
        OTHERS          = 7.
    IF sy-subrc = 1.
      RETURN.
    ENDIF.
    ASSERT sy-subrc = 0.

  ENDMETHOD.

  METHOD run.

    create( iv_devclass ).

    scan( iv_source ).

    DATA(lv_pool_include) = cl_oo_classname_service=>get_interfacepool_name( ms_class-clsname ).
    cl_where_used_list_utilities=>update_index_in_background( lv_pool_include ).

  ENDMETHOD.

  METHOD update_meta.

    DATA: lo_update     TYPE REF TO cl_oo_class_section_source,
          ls_clskey     TYPE seoclskey,
          lv_scan_error TYPE seox_boolean.


    ls_clskey-clsname = ms_class-clsname.

    CREATE OBJECT lo_update
      EXPORTING
        clskey                        = ls_clskey
        exposure                      = iv_exposure
        state                         = 'A'
        source                        = it_source
        suppress_constrctr_generation = seox_true
      EXCEPTIONS
        class_not_existing            = 1
        read_source_error             = 2
        OTHERS                        = 3.
    IF sy-subrc <> 0.
      BREAK-POINT.
    ENDIF.

    lo_update->set_dark_mode( seox_true ).
*    lo_update->set_amdp_support( amdp_support_enabled ).
    lo_update->scan_section_source(
      RECEIVING
        scan_error             = lv_scan_error
      EXCEPTIONS
        scan_abap_source_error = 1
        OTHERS                 = 2 ).
    IF sy-subrc <> 0 OR lv_scan_error = abap_true.
      BREAK-POINT.
    ENDIF.

* this will update the SEO* database tables
    lo_update->revert_scan_result( ).

    IF iv_exposure = seoc_exposure_public.
      generate_classpool( ).
    ENDIF.

  ENDMETHOD.

  METHOD init_scanner.

    DATA: lt_source TYPE seop_source_string.

    SPLIT iv_source AT |\n| INTO TABLE lt_source.

    ro_scanner = cl_oo_source_scanner_class=>create_class_scanner(
      clif_name = ms_class-clsname
      source    = lt_source ).
    ro_scanner->scan( ).

  ENDMETHOD.

  METHOD determine_method_include.

    DATA: ls_mtdkey TYPE seocpdkey.


    ls_mtdkey-clsname = ms_class-clsname.
    ls_mtdkey-cpdname = iv_method.

    cl_oo_classname_service=>get_method_include(
      EXPORTING
        mtdkey              = ls_mtdkey
      RECEIVING
        result              = rv_program
      EXCEPTIONS
        method_not_existing = 1 ).
    IF sy-subrc = 0.
      RETURN.
    ENDIF.

    CALL FUNCTION 'SEO_METHOD_GENERATE_INCLUDE'
      EXPORTING
        suppress_mtdkey_check          = seox_true
        mtdkey                         = ls_mtdkey
      EXCEPTIONS
        not_existing                   = 1
        model_only                     = 2
        include_existing               = 3
        method_imp_not_generated       = 4
        method_imp_not_initialised     = 5
        _internal_class_not_existing   = 6
        _internal_method_overflow      = 7
        cancelled                      = 8
        method_is_abstract_implemented = 9
        method_is_final_implemented    = 10
        internal_error_insert_report   = 11
        OTHERS                         = 12.
    IF sy-subrc <> 0.
      BREAK-POINT.
    ENDIF.

    cl_oo_classname_service=>get_method_include(
      EXPORTING
        mtdkey = ls_mtdkey
      RECEIVING
        result = rv_program ).

  ENDMETHOD.

  METHOD scan.

    DATA(lo_scanner) = init_scanner( iv_source ).


    DATA(lt_source) = lo_scanner->get_public_section_source( ).
    DATA(lv_program) = cl_oo_classname_service=>get_pubsec_name( ms_class-clsname ).
    INSERT REPORT lv_program FROM lt_source.
    ASSERT sy-subrc = 0.
    update_meta(
      iv_exposure = seoc_exposure_public
      it_source   = lt_source ).

* todo:
*   protected
*   private

* methods
    DATA(lt_methods) = lo_scanner->get_method_implementations( ).

    LOOP AT lt_methods INTO DATA(lv_method).
      lt_source = lo_scanner->get_method_impl_source( lv_method ).
      lv_program = determine_method_include( lv_method ).

      INSERT REPORT lv_program FROM lt_source.
      ASSERT sy-subrc = 0.
    ENDLOOP.

  ENDMETHOD.

  METHOD check.
* todo
  ENDMETHOD.

  METHOD generate_classpool.

    DATA: ls_clskey TYPE seoclskey.

    ls_clskey-clsname = ms_class-clsname.

    CALL FUNCTION 'SEO_CLASS_GENERATE_CLASSPOOL'
      EXPORTING
        clskey                        = ls_clskey
        suppress_corr                 = seox_true
      EXCEPTIONS
        not_existing                  = 1
        model_only                    = 2
        class_pool_not_generated      = 3
        class_stment_not_generated    = 4
        locals_not_generated          = 5
        macros_not_generated          = 6
        public_sec_not_generated      = 7
        protected_sec_not_generated   = 8
        private_sec_not_generated     = 9
        typeref_not_generated         = 10
        class_pool_not_initialised    = 11
        class_stment_not_initialised  = 12
        locals_not_initialised        = 13
        macros_not_initialised        = 14
        public_sec_not_initialised    = 15
        protected_sec_not_initialised = 16
        private_sec_not_initialised   = 17
        typeref_not_initialised       = 18
        _internal_class_overflow      = 19
        OTHERS                        = 20.
    IF sy-subrc <> 0.
      BREAK-POINT.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
