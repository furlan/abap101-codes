*&---------------------------------------------------------------------*
*& Report zwait_timeout
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zwait_timeout.

DATA functioncalldone TYPE c LENGTH 1.
DATA return TYPE i.
CONSTANTS done TYPE c LENGTH 1 VALUE 'X'.

PARAMETERS ptimeout TYPE i DEFAULT 10.
PARAMETERS pwait    TYPE i DEFAULT 20.
PARAMETERS pvalue   TYPE i DEFAULT 2.

SELECTION-SCREEN COMMENT /1(83) text.

AT SELECTION-SCREEN OUTPUT.
  text = 'If PWAIT > PTIMEOUT == error message, otherwise calculate PVALUE * 2'.

START-OF-SELECTION.

  CALL FUNCTION 'Z_WAIT_ASYNC'
    STARTING NEW TASK 'taskname'
    PERFORMING on_returning ON END OF TASK
    EXPORTING
      im_wait  = pwait
      im_value = pvalue.

  DATA output TYPE REF TO if_demo_output.
  WAIT FOR ASYNCHRONOUS TASKS UNTIL functioncalldone = done UP TO ptimeout SECONDS.
  IF sy-subrc EQ 0.
    output = cl_demo_output=>new( )->write( 'Return from function module: ' )->write( return ).
  ELSE.
    output = cl_demo_output=>new( )->write( 'Timeout error.' ).
  ENDIF.

  output->display( ).

FORM on_returning USING taskname.
  RECEIVE RESULTS FROM FUNCTION 'Z_WAIT_ASYNC' IMPORTING ex_return = return.
  functioncalldone = done.
ENDFORM.
