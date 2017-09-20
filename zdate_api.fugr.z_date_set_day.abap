FUNCTION z_date_set_day.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IM_DAY) TYPE  I
*"  CHANGING
*"     REFERENCE(CH_DATE) TYPE  SCALS_DATE
*"  EXCEPTIONS
*"      INVALID_DATE
*"----------------------------------------------------------------------

  DATA month_end TYPE i.

  CASE ch_date-month.

    WHEN 1 OR 3 OR 5 OR 7 OR 8 OR 10 OR 12.
      month_end = 31.
    WHEN 4 OR 6 OR 9 OR 11.
      month_end = 30.
    WHEN 2.
      month_end = 28.
    WHEN OTHERS.
      RAISE invalid_date.
  ENDCASE.

  IF im_day LT 1 OR im_day GT month_end.
    RAISE invalid_date.
  ELSE.
    ch_date-day = im_day.
  ENDIF.


ENDFUNCTION.
