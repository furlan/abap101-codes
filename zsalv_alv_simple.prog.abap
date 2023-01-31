*&---------------------------------------------------------------------*
*& Report ZSALV_ALV_SIMPLE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsalv_alv_simple.

DATA: bookings TYPE TABLE OF sbook,
      booking  TYPE sbook.

SELECT * FROM sbook INTO TABLE bookings.

DATA: alv_table TYPE REF TO cl_salv_table,
      functions TYPE REF TO cl_salv_functions_list.

CALL METHOD cl_salv_table=>factory
  IMPORTING
    r_salv_table = alv_table
  CHANGING
    t_table      = bookings.

functions = alv_table->get_functions( ).
functions->set_all( abap_true ).


CALL METHOD alv_table->display.
