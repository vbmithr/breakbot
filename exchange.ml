open Common

type api_kind = Streaming_TCP | Streaming_WebSocket | REST

type t =
    {
      name: string;
      currencies: Currency.t list;
      apis: api_kind list
    }
