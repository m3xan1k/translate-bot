#lang racket

(require net/url)
(require json)

(define tg-api-token (getenv "TG_API_TOKEN"))
(define tg-base-url 
  (format "https://api.telegram.org/bot~a" tg-api-token))
(define tg-update-limit 1)
(define tg-timeout 60)


(define (port->jsexpr port)
  (string->jsexpr (port->string port)))


(define (get-updates #:last-update-id (last-update-id null))
  (let
    ((params (format "?limit=~a&timeout=~a" tg-update-limit tg-timeout)))
        (if (null? last-update-id)
          (port->jsexpr (get-pure-port
            (string->url (format "~a/getUpdates~a" tg-base-url params))))
          (port->jsexpr (get-pure-port
            (string->url
              (format "~a/getUpdates~a&offset=~a"
                tg-base-url params (+ tg-update-limit last-update-id))))))))


(define (send-message #:chat-id chat-id #:text text)
  (port->jsexpr (post-pure-port
    (string->url (format "~a/sendMessage" tg-base-url))
    (jsexpr->bytes
      (hasheq 'chat_id chat-id 'text text 'parse_mode "MarkdownV2"))
    (list "Content-type: application/json"))))


(define help-message
  (format "~a~n~a~n~a~n~a~n~a~n"
    "Using:"
    "`--source -s <language code>`"
    "`--target -t <language code>`"
    "Example:"
    "`-s ru -t en Привет`"))


(define (listen)
  (let ((tg-updates (get-updates)))
    (displayln tg-updates)))


(listen)
