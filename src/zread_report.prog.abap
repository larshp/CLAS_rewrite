*&---------------------------------------------------------------------*
*& Report ZREAD_REPORT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zread_report.

START-OF-SELECTION.
  PERFORM run.

FORM run.

  SELECT * FROM reposrc INTO TABLE @DATA(lt_reposrc)
    WHERE progname LIKE 'ZCL_FOOBAA%'.

  LOOP AT lt_reposrc INTO DATA(ls_reposrc).
    PERFORM read USING ls_reposrc-progname.
  ENDLOOP.

ENDFORM.

FORM read USING p_program TYPE programm.

  DATA: source TYPE TABLE OF string.

  WRITE: / p_program.
  READ REPORT p_program INTO source.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  LOOP AT source INTO DATA(src).
    WRITE: / |  |, src.
  ENDLOOP.

ENDFORM.
