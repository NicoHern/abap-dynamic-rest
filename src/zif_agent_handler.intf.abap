INTERFACE zif_agent_handler PUBLIC.

  DATA mo_request  TYPE REF TO if_http_request.
  DATA mo_response TYPE REF TO if_http_response.
  DATA mv_body     TYPE string.

  METHODS initialize
    IMPORTING
      io_request  TYPE REF TO if_http_request
      io_response TYPE REF TO if_http_response.

  METHODS get_supported_methods
    RETURNING
      VALUE(rt_methods) TYPE string_table.

  METHODS check_authorization
    IMPORTING
      iv_auth_object        TYPE xuobject
    RETURNING
      VALUE(rv_authorized) TYPE abap_bool.

ENDINTERFACE.
