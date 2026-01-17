CLASS zcl_agent_ping DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC
  INHERITING FROM zcl_agent_handler_base.

  PUBLIC SECTION.
    METHODS handle_ping.
    METHODS handle_echo.

ENDCLASS.



CLASS zcl_agent_ping IMPLEMENTATION.

  METHOD handle_ping.
    DATA: lv_timestamp TYPE string,
          lv_response  TYPE string,
          lv_datum     TYPE char10,
          lv_uzeit     TYPE char8.

    lv_datum = sy-datum.
    lv_uzeit = sy-uzeit.

    CONCATENATE lv_datum ' ' lv_uzeit INTO lv_timestamp.

    CONCATENATE
      '"message":"pong",'
      '"timestamp":"' lv_timestamp '",'
      '"system":"' sy-sysid '",'
      '"client":"' sy-mandt '"'
      INTO lv_response.

    send_success( lv_response ).
  ENDMETHOD.


  METHOD handle_echo.
    DATA: lv_message  TYPE string,
          lv_response TYPE string,
          lv_escaped  TYPE string.

    lv_message = parse_json_param( 'message' ).

    IF lv_message IS INITIAL.
      send_error( iv_message = 'Missing parameter: message' iv_code = 400 ).
      RETURN.
    ENDIF.

    lv_escaped = escape_json_string( lv_message ).

    CONCATENATE '"echo":"' lv_escaped '"' INTO lv_response.

    send_success( lv_response ).
  ENDMETHOD.

ENDCLASS.
