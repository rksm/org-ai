# Changelog

## [0.4.7] - 2024-05-17
### Changed
Add new openai models to `org-ai-chat-models` and check if a model name is mistyped. Requested by [#110](https://github.com/rksm/org-ai/issues/110).

## [0.4.6] - 2024-05-13
### Added
Option to auto-fill paragraphs on insertion from the ai: With `(setq org-ai-auto-fill t)` set this behavior will invoke `fill-paragraph` on insertion when not inside a code block.


## [0.4.5] - 2024-05-08
### Changed
-  fix(org-ai-openai-image): make generating images work with Org 9.7. Thank you @JakDar!


## [0.4.4] - 2024-05-08
### Added
- org-ai blocks now accept a service parameter to switch between different endpoints and service-specific behavior:

```org
#+begin_ai :service anthropic
...
#+end_ai
```

The same can be achieved globally by setting `org-ai-service`. Possible values are listed [here](https://github.com/rksm/org-ai/blob/master/org-ai-openai.el#L112).

- support for the Anthropic Claude API. See [the readme](https://github.com/rksm/org-ai#anthropic--claude)

- support for the perplexity.ai API. See [the readme](https://github.com/rksm/org-ai#perplexityai)


## [0.4.3] - 2024-01-03
### Added
- org-ai block attributes can now be set with org drawer propert (see https://github.com/rksm/org-ai/pull/99 https://github.com/rksm/org-ai#for-chatgpt) Thank you @doctorguile!
- Support for DALL·E-3! Should now work out of the box. org-ai image blocks now also support attributes like `:model`, `:quality`, and `style`. See the [README](https://github.com/rksm/org-ai#for-dall-e) for details.

### Changed
- Fix for selecting text in `org-ai-prompt-in-new-buffer`


## [0.4.2] - 2023-12-26

### Added
- Added `org-ai-prompt-in-new-buffer` command to query for a prompt and then run that in a new buffer.

### Changed
- Mark `org-ai-default-inject-sys-prompt-for-all-messages` as deprecated. No need for this any longer.
- Fix inserting system prompt, thx @doctorguile!
- Added notes about how to use Azure and Azure specific auth, thx @tillydray

## [0.4.1] - 2023-09-10
### Changed
- Introduced `org-ai-oobabooga-create-prompt-function` that can be used to customize the prompt creation for local LLMs. It defaults to `org-ai-oobabooga-create-prompt-default` which uses the values of the variables `org-ai-oobabooga-system-prefix`, `org-ai-oobabooga-user-prefix` and `org-ai-oobabooga-assistant-prefix` to assemble the prompt text. Example:
```elisp
(funcall org-ai-oobabooga-create-prompt-function
         [(:role system :content "system")
          (:role user :content "hello")
          (:role assistant :content "world")])
;; => "PROMPT: system\n\nYou: hello\n\nAssistant: world\n\n"
```

## [0.4.0] - 2023-09-05
### Added
- Add support for local LLMs via [oobabooga/text-generation-webui](https://github.com/oobabooga/text-generation-webui). See [issue #78](https://github.com/rksm/org-ai/issues/78).

## [0.3.13] - 2023-08-20
### Changed
- add macro `org-ai--org-element-with-disabled-cache`. `org-element-with-disabled-cache` is not available pre org-mode 9.6.6. (resolves https://github.com/rksm/org-ai/issues/77)

## [0.3.12] - 2023-08-19
### Changed
- Fix `org-ai-global-mode` definition (https://github.com/rksm/org-ai/issues/76)

## [0.3.11] - 2023-08-03
### Changed
- Add `(require 'org-macs)` for `org-element-with-disabled-cache` that was added in 0.3.10

## [0.3.10] - 2023-08-02
### Changed
- Allow dynamic `org-ai-on-region-file` (https://github.com/rksm/org-ai/pull/71) (i.e you can add a function to handle it)
- Emacs 27 as minimal required version
- call `org-element-cache-reset` after insertion to avoid org warnings (see https://github.com/rksm/org-ai/issues/16 and https://github.com/rksm/org-ai/issues/63)
- Double check special block properties when extracting org-ai block.

## [0.3.9] - 2023-06-23
### Added
- Add `gpt-3.5-turbo-16k` to `org-ai-chat-models`
### Changed
- fix detecting role vs content in the API response that has changed with the last OpenAI update (https://github.com/rksm/org-ai/issues/58)

## [0.3.8] - 2023-06-08
### Added
- [Support](https://github.com/rksm/org-ai/pull/57) for stable diffusion based image generation through [stable-diffusion-webui](https://github.com/AUTOMATIC1111/stable-diffusion-webui) (Huge Thanks [@yaak0](https://github.com/yaak0)!).

```
#+begin_ai :sd-image
<PROMPT>
#+end_ai
```

### Changed
- Better [noweb](https://orgmode.org/manual/Noweb-Reference-Syntax.html) [support](https://github.com/rksm/org-ai/pull/51). (Big Thank You [@togakangaroo](https://github.com/togakangaroo)!)

## [0.3.7] - 2023-06-04
### Changed
- make token retrieval via `auth-source` on demand

## [0.3.6] - 2023-05-30
### Changed
- fix for `string-equal-ignore-case` issue (https://github.com/rksm/org-ai/issues/53)
- documented use of greader/espeak (https://github.com/rksm/org-ai/issues/49)

## [0.3.5] - 2023-05-11
### Added
- noweb support (https://github.com/rksm/org-ai/pull/46)

## [0.3.4] - 2023-05-11
### Changes
- Bind canceling `org-ai-on-project--confirm-selection` to `C-c C-k` instead of `C-c k`.

## [0.3.3] - 2023-05-11
### Changes
- Ensure that request errors are shown (fix [#43](https://github.com/rksm/org-ai/issues/43))
- Use markers instead of positions when inserting streamed content

## [0.3.2] - 2023-04-18

### Changes
- For the global mode, use a prefix-map. `org-ai-global-prefix-map`, it is bound to `C-c M-a` by default but can be easily bound to another key.
- don't show `org-ai-global-mode` in the mode-line
- no `show-trailing-whitespace` for org-ai-on-project
- load `org-ai-talk` by default

## [0.3.1] - 2023-04-17
### Added
- `org-ai-on-project`: Load by default.

### Changed
- with consecutive `org-ai-kill-region-at-point` invocations, append kill in right order ([#32](https://github.com/rksm/org-ai/issues/32))

## [0.3.0] - 2023-04-17
### Added
- `org-ai-on-project`: Offers a new method for running prompts and modifying multiple files at once.

### Changed
- `org-ai-on-region` (and related functions such as `org-ai-refactor`) do not quote user input with "> " anymore as this could be repeated in the output (see [#30](https://github.com/rksm/org-ai/issues/30))
- `org-ai-on-region` now outputs both prompt and answer into an `org-mode` buffer. This allows to easily edit the answer and re-run the prompt with `C-c C-c` (see [#29](https://github.com/rksm/org-ai/issues/29)). You can customize the variable `org-ai-on-region-file` to specify a file to store the conversations in. E.g. `(setq org-ai-on-region-file (expand-file-name "org-ai-on-region.org" org-directory))`. New prompts will be appended.

## [0.2.6] - 2023-04-07
### Changed
- Deactivating read-only mode when reusing the `*org-ai-on-region*` buffer. This blocked repeated on-region requests if the buffer was still open.

## [0.2.5] - 2023-04-07
- Another attempt to fix unicode / multi-byte inputs. The string encoding we had before 0.2.3 was correct, but I missed that the entirety of the request body needs to be encoded as utf-8, including the headers need to be utf-8. In my case my openai auth token was not utf-8, and so i got a `Multibyte text in HTTP request` and wrongly assumed this is due to the content. This should be fixed now.

## [0.2.4] - 2023-04-04
### Changed
- Make 'org-ai-summarize' treat the region as text not as code (https://github.com/rksm/org-ai/pull/22)
- fix "Error on checkbox C-c C-c after org-ai loaded" (https://github.com/rksm/org-ai/issues/23)

## [0.2.3] - 2023-04-04
### Changed
- correctly encode UTF-8 strings when sending them to the API. This should fix non-ascii characters and multi-byte characters such as emojis in the input.
- start markdown codeblock AI responses on their own line (see https://github.com/rksm/org-ai/issues/17#issuecomment-1494970605)

## [0.2.2] - 2023-03-31
### Changed
- add `"ai"` to `org-protecting-blocks` to fix the block syntax highlighting.

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
### Removed
- Make greader dependency optional

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
