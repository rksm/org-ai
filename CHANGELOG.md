# Changelog

## [0.2.1] - 2023-03-30
### Added
- `org-ai-switch-chat-model`
### Changed
- No max-tokens in ai snippet
- modified global shortcuts:
    - `C-c M-a r`: `org-ai-on-region`
    - `C-c M-a c`: `org-ai-refactor-code`
    - `C-c M-a s`: `org-ai-summarize`
    - `C-c M-a m`: `org-ai-switch-chat-model`
    - `C-c M-a !`: `org-ai-open-request-buffer`
    - `C-c M-a $`: `org-ai-open-account-usage-page`
    - `C-c M-a t`: `org-ai-talk-input-toggle`
    - `C-c M-a T`: `org-ai-talk-output-toggle`


## [0.2.0] - 2023-03-30
### Added

#### Speech input/output. Talk with your AI!
- In org-mode / `#+begin_ai..#+end_ai` blocks:
  - `C-c r` to record and transcribe speech via whisper.el in org blocks.
- Everywhere else:
    - Enable speech input with `org-ai-talk-input-toggle` for other commands (see below).
- Enable speech output with `org-ai-talk-output-enable`. Speech output uses os internal speech synth (macOS) or `espeak` otherwise.

#### Non-org-mode commands
- `org-ai-prompt`: prompt the user for a text and then print the AI's response in current buffer.
- `org-ai-on-region`: Ask a question about the selected text or tell the AI to do something with it.
- `org-ai-summarize`: Summarize the selected text.
- `org-ai-explain-code`: Explain the selected code.
- `org-ai-refactor-code`: Tell the AI how to change the selected code, a diff buffer will appear with the changes.

#### Other improvements
- In org-mode / `#+begin_ai..#+end_ai` blocks:
    - Press `C-c <backspace>` (`org-ai-kill-region-at-point`) to remove the chat part under point.
    - `org-ai-mark-region-at-point` will mark the region at point.
    - `org-ai-mark-last-region` will mark the last chat part.

#### Other commands
- `org-ai-open-account-usage-page` show how much money you burned.
- `org-ai-install-yasnippets` install snippets for `#+begin_ai..#+end_ai` blocks.
- `org-ai-open-request-buffer` for debugging, open the request buffer.

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
