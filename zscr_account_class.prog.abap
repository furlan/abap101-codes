*&---------------------------------------------------------------------*
*& Report zscr_account_class
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zscr_account_class.

CLASS account DEFINITION.

  PUBLIC SECTION.

    METHODS constructor IMPORTING account_no TYPE zbank_account-account_no.

    METHODS get_balance RETURNING VALUE(balance) TYPE zbank_account-balance.

    METHODS deposit IMPORTING amount TYPE zbank_account-balance.

    METHODS withdrawal IMPORTING  amount TYPE zbank_account-balance
                       EXCEPTIONS insufficient_found.

  PRIVATE SECTION.

    DATA account_no TYPE zbank_account-account_no.
    DATA name1      TYPE zbank_account-name1.
    DATA balance    TYPE zbank_account-balance.

ENDCLASS.

CLASS account IMPLEMENTATION.

  METHOD constructor.
    SELECT SINGLE name1 balance
       INTO ( me->name1, me->balance )
       FROM zbank_account
       WHERE account_no = account_no.
    IF sy-subrc NE 0.
      "do nothing...
    ENDIF.
  ENDMETHOD.

  METHOD get_balance.
    balance = me->balance.
  ENDMETHOD.

  METHOD deposit.
    ADD amount TO me->balance.
  ENDMETHOD.

  METHOD withdrawal.
    IF amount <= me->balance.
      SUBTRACT amount FROM me->balance.
    ELSE.
      RAISE insufficient_found.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
