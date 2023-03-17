# Changelog

## [0.1.4] - 2023-03-17
### Added
- Support for retrieving `org-ai-openai-api-token` from `authinfo` file.

## [0.1.3] - 2023-03-13
### Added
- org-ai yasnippets
### Changes
- Correctly utf decode/encode input/output.
- Set mark before inserting output. This allows to quickly select the output with `C-x C-x`.
- Fix parsing messages that contain a `:`.

## [0.1.2] - 2023-03-08
### Fix ChatGPT prompts
- Add `org-ai-default-chat-system-prompt`, `org-ai-default-inject-sys-prompt-for-all-messages`, `:sys-everywhere` option and allow specifying custom per-block `[SYS]:` system prompts.
- Correctly assign system, user, assistant roles (see https://platform.openai.com/docs/guides/chat/introduction). Before I mixed up system/assistant roles.

## [0.1.1] - 2023-03-06
### Initial release
`#+begin_ai...#+end_ai` blocks that can be used for:
- chatgpt input / output
- "normal" text completion with older gpt models
- image genration (text -> image)

`org-ai-image-variation` command to generate variations of an image.
