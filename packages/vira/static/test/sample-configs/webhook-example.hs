\ctx pipeline ->
  pipeline
    { postBuild.webhooks =
        [ WebhookConfig
            { url     = "https://hooks.slack.com/services/$SLACK_WEBHOOK_TOKEN"
            , method  = POST
            , headers = [("Content-Type", "application/json")]
            , body    = Just "{\"text\": \"✅ $VIRA_BRANCH @ $VIRA_COMMIT_ID built successfully\"}"
            }
        ]
    }
