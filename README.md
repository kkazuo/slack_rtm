# Slack RTM Receiver

## Example

Receive all message from ```rtm.start```:

    slack-rtm -token _your_token_

Show message text:

    slack-rtm -token _your_token_ -type message | jq '.text'
