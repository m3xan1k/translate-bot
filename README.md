# translate-bot

Source code for https://t.me/libre_translator_bot

## Deploy

- Rename you_need.env to .env
- Fill .env with your bot api token
- Run `docker-compose up -d`
- Enjoy

## Using

Required options are `-s(source) <language code>` and `-t(target) <language code>`


Examples:

```
-s ru -t en Привет
> Hello
-s en -t ru big
> Большой
```

List available languages:

```
/languages
> ar/Arabic
eu/Basque
bn/Bengali
en-GB/English
pt-BR/Portuguese
bg/Bulgarian
ca/Catalan
chr/Cherokee
hr/Croatian
cs/Czech
da/Danish
nl/Dutch
en/English
et/Estonian
fil/Filipino
fi/Finnish
fr/French
de/German
el/Greek
gu/Gujarati
iw/Hebrew
hi/Hindi
hu/Hungarian
is/Icelandic
id/Indonesian
it/Italian
ja/Japanese
kn/Kannada
ko/Korean
lv/Latvian
lt/Lithuanian
ms/Malay
ml/Malayalam
mr/Marathi
no/Norwegian
pl/Polish
pt-PT/Portuguese
ro/Romanian
ru/Russian
sr/Serbian
zh-CN/Chinese
sk/Slovak
sl/Slovenian
es/Spanish
sw/Swahili
sv/Swedish
ta/Tamil
te/Telugu
th/Thai
zh-TW/Chinese
tr/Turkish
ur/Urdu
uk/Ukrainian
vi/Vietnamese
cy/Welsh
```

## LICENSE

DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
                   Version 2, December 2004

Copyright (C) 2004 Sam Hocevar <sam@hocevar.net>

Everyone is permitted to copy and distribute verbatim or modified
copies of this license document, and changing it is allowed as long
as the name is changed.

           DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
  TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION

 0. You just DO WHAT THE FUCK YOU WANT TO.

## TODO:

- [x] Docker
- [ ] Code comments
- [x] Documentation for self-host deploy
