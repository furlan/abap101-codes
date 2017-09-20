*&---------------------------------------------------------------------*
*& Report zscr_mock_bapi
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zscr_mock_bapi.

TYPE-POOLS abap.

INTERFACE phone_message.
  METHODS has_message_sender IMPORTING input_phone     TYPE bapiadtel-telephone
                             RETURNING VALUE(is_valid) TYPE abap_bool.
ENDINTERFACE.

INTERFACE checker.
  METHODS phone_checker IMPORTING bapi_get        TYPE REF TO zif_bupa_address_getdetail
                                  bus_partner     TYPE bapibus1006_head-bpartner
                                  phone_check     TYPE bapiadtel-telephone
                        RETURNING VALUE(is_valid) TYPE abap_bool.
  METHODS message_send_checker IMPORTING sender_check    TYPE REF TO phone_message
                                         phone_check     TYPE bapiadtel-telephone
                               RETURNING VALUE(is_valid) TYPE abap_bool.
ENDINTERFACE.

CLASS contact_checker DEFINITION.
  PUBLIC SECTION.
    INTERFACES checker.
ENDCLASS.

CLASS contact_checker IMPLEMENTATION.
  METHOD checker~phone_checker.
    DATA result_bapiadtel TYPE bapiadtel_t.
    DATA result_return TYPE bapiret2_t.
    FIELD-SYMBOLS <bapiadtel> TYPE bapiadtel.

    bapi_get->bapi_bupa_address_getdetail(
        EXPORTING businesspartner = bus_partner
        IMPORTING bapiadtel_table = result_bapiadtel
                  return_table    = result_return ).

    TRY.
        IF result_bapiadtel[ 1 ]-telephone EQ phone_check.
          is_valid = abap_true.
        ELSE.
          is_valid = abap_false.
        ENDIF.
      CATCH cx_root.
        is_valid = abap_false.
    ENDTRY.
  ENDMETHOD.

  METHOD checker~message_send_checker.
    is_valid = sender_check->has_message_sender( phone_check ).
  ENDMETHOD.
ENDCLASS.

CLASS contact_checker_test DEFINITION FOR TESTING RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    DATA checker TYPE REF TO contact_checker.
    METHODS setup.
    METHODS get_bapi_caller IMPORTING telephone          TYPE  bapiadtel-telephone
                            RETURNING VALUE(bapi_caller) TYPE REF TO zif_bupa_address_getdetail.
    METHODS should_be_valid FOR TESTING.
    METHODS should_not_be_valid FOR TESTING.
    METHODS should_capable_to_send_message FOR TESTING.
ENDCLASS.

CLASS contact_checker_test IMPLEMENTATION.
  METHOD setup.
    CREATE OBJECT me->checker.
  ENDMETHOD.

  METHOD get_bapi_caller.
    FIELD-SYMBOLS <bapiadtel> TYPE bapiadtel.
    DATA mock_bapiadtel TYPE bapiadtel_t.
    DATA mock_return    TYPE bapiret2_t.

    DATA mocker TYPE REF TO zif_mocka_mocker.
    mocker = zcl_mocka_mocker=>zif_mocka_mocker~mock( 'zif_bupa_address_getdetail' ).

    APPEND INITIAL LINE TO mock_bapiadtel ASSIGNING <bapiadtel>.
    <bapiadtel>-telephone = telephone.

    mocker->method( 'BAPI_BUPA_ADDRESS_GETDETAIL' )->with( i_p1 = '0001' )->exports( i_p1 = mock_bapiadtel i_p2 = mock_return ).
    bapi_caller ?= mocker->generate_mockup( ).
  ENDMETHOD.

  METHOD should_be_valid.
    DATA(bapi_caller) = me->get_bapi_caller( '5551239876' ).
    DATA(actual) = me->checker->checker~phone_checker( bapi_get = bapi_caller bus_partner = '0001' phone_check = '5551239876' ).
    cl_abap_unit_assert=>assert_equals( exp = abap_true act = actual ).
  ENDMETHOD.

  METHOD should_not_be_valid.
    DATA(bapi_caller) = me->get_bapi_caller( '5556667777' ).
    DATA(actual) = me->checker->checker~phone_checker( bapi_get = bapi_caller bus_partner = '0001' phone_check = '5551239876' ).
    cl_abap_unit_assert=>assert_equals( exp = abap_false act = actual ).
  ENDMETHOD.

  METHOD should_capable_to_send_message.
    DATA message_checker TYPE REF TO phone_message.

    DATA(actual) = me->checker->checker~message_send_checker( sender_check = message_checker
                                                              phone_check = '5551239876' ).
    cl_abap_unit_assert=>assert_equals( exp = abap_true act = actual ).
  ENDMETHOD.
ENDCLASS.
























"EOF
