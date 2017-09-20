class ZCX_BAD_EXECUTION definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  constants ZCX_BAD_EXECUTION type SOTR_CONC value '001D4FFC4B1E1ED6BCF8945C9BA312C9' ##NO_TEXT.
  constants ZERO_DIVISION type SOTR_CONC value '001D4FFC4B1E1ED6BCF8945C9BA332C9' ##NO_TEXT.

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_BAD_EXECUTION IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
TEXTID = TEXTID
PREVIOUS = PREVIOUS
.
 IF textid IS INITIAL.
   me->textid = ZCX_BAD_EXECUTION .
 ENDIF.
  endmethod.
ENDCLASS.
