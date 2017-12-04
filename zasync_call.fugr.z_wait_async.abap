FUNCTION z_wait_async.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IM_WAIT) TYPE  I
*"     REFERENCE(IM_VALUE) TYPE  I
*"  EXPORTING
*"     REFERENCE(EX_RETURN) TYPE  I
*"----------------------------------------------------------------------

  WAIT UP TO im_wait SECONDS.
  ex_return = im_value * 2.

ENDFUNCTION.
