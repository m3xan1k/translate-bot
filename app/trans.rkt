#lang racket

(require net/url)
(require json)

(define tg-api-token (getenv "TG_API_TOKEN"))
(define tg-base-url 
  (format "https://api.telegram.org/bot~a" tg-api-token))
(define tg-update-limit 1)
(define tg-timeout 60)

(define translate-base-url "https://translate.googleapis.com/translate_a/single")


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


(define (parse-message text)
  (match (string-split text " ")
    [(list "-s" source "-t" target word) (hash 'source source 'target target 'word word)]
    [(list "-t" target "-s" source word) (hash 'source source 'target target 'word word)]
    [(list "/languages") (list "/languages")]
    [else null]))


(define (translate parsed-message)
 (let*
    ([source (hash-ref parsed-message 'source)]
     [target (hash-ref parsed-message 'target)]
     [word (hash-ref parsed-message 'word)]
     [answer-list (port->jsexpr (get-pure-port
        (string->url
          (format "~a?sl=~a&tl=~a&q=~a&client=gtx&dt=t"
            translate-base-url source target word))
          (list "Content-type: Application/json")))])
    (first (first (first answer-list)))))


(define (normalize-lang-item lst-item)
  (format "~a/~a" (hash-ref lst-item 'code) (hash-ref lst-item 'name)))


(define (get-languages)
  (let
    ([answer-list (port->jsexpr (get-pure-port
        (string->url "https://libretranslate.com/languages")
        (list "Accept: application/json")))])
    (format "```~a```" (string-join (map normalize-lang-item answer-list) "\n"))))


(define (create-message parsed-message)
  (cond [(hash? parsed-message) (translate parsed-message)]
        [(null? parsed-message) help-message]
        [(list? parsed-message) (get-languages)]))


(define (listen #:last-update-id (last-update-id null))
  (let ((tg-updates (get-updates #:last-update-id last-update-id)))
    (if (and (hash-ref tg-updates 'ok) (not (null? (hash-ref tg-updates 'result))))
      (let* ([result (first (hash-ref tg-updates 'result))]
             [message (if (hash-has-key? result 'message)
                        (hash-ref result 'message)
                        (hash-ref result 'edited_message))]
             [text (hash-ref message 'text)]
             [update-id (hash-ref (first (hash-ref tg-updates 'result)) 'update_id)]
             [chat-id (hash-ref (hash-ref message 'chat) 'id)]
             [message (create-message (parse-message text))])
        (send-message #:chat-id chat-id #:text message)
        (listen #:last-update-id update-id))
      (if (hash-ref tg-updates 'ok)
        (listen)
        ;TODO
        ;if not ok (404)
        null))))


(listen)
