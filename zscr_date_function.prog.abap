*&---------------------------------------------------------------------*
*& Report zscr_date_function
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zscr_date_function.

DATA initial_date TYPE scals_date.
DATA final_date TYPE scals_date.

START-OF-SELECTION.

  initial_date-month = 11.

  CALL FUNCTION 'Z_DATE_SET_DAY'
    EXPORTING
      im_day       = 10
    CHANGING
      ch_date      = initial_date
    EXCEPTIONS
      invalid_date = 1
      OTHERS       = 2.

  final_date-month = 12.

  CALL FUNCTION 'Z_DATE_SET_DAY'
    EXPORTING
      im_day       = 20
    CHANGING
      ch_date      = final_date
    EXCEPTIONS
      invalid_date = 1
      OTHERS       = 2.
