#!/bin/bash

/usr/bin/curl -X POST https://austral2024.zulip.rebelare.com/api/v1/messages \
    -u GoogleCloud-bot@austral2024.zulip.rebelare.com:UggjKUEzYIkt3yk1HBOoALaNhH5oekOU \
    --data-urlencode type=direct \
    --data-urlencode 'to=jenicolau@gmail.com' \
    --data-urlencode 'content= '"$1"'.'
