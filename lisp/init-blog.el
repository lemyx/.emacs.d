;;; init-blog.el --- Blog settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-static-blog for blog
(use-package org-static-blog
  :ensure t
  :config
  (setq org-static-blog-publish-url "https://lemyx.github.io/"
        org-static-blog-publish-title "LEMYX")

  (setq org-static-blog-publish-directory "~/blog"          ; Store published HTML files
        org-static-blog-posts-directory   "~/org/blog/posts/"   ; Store published org files
        org-static-blog-drafts-directory  "~/org/blog/drafts/") ; Store unpublished org files

  (setq org-static-blog-index-file "index.html"
        org-static-blog-archive-file "archive.html"         ; Archive page lists all posts as headlines.
        org-static-blog-tags-file "tags.html"               ; Tags page lists all posts as headlines.
        org-static-blog-rss-file "rss.xml")

  (setq org-static-blog-index-length 100                    ; Number of articles to include on index page.
        org-static-blog-enable-tags t                       ; Show tags below posts, and generate tag pages.
        org-static-blog-use-preview t                       ; Use preview versions of posts on multipost pages.
        org-static-blog-preview-ellipsis "(...)"            ; appended HTML to the preview as hidden part
        org-static-blog-preview-link-p t                    ; Make preview-ellipsis a link to article's page
        org-static-blog-preview-date-first-p nil)           ; Print post dates before title in preview

  (setq org-export-with-toc nil
        org-export-with-section-numbers t)

  (setq org-static-blog-page-header                         ; HTML to put in the <head> of each page.
        "<meta name=\"author\" content=\"lemyx\">
<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
<link href= \"static/style.css\" rel=\"stylesheet\" type=\"text/css\" />
<script src=\"static/toc.js\" type=\"text/javascript\" defer></script>
<script src=\"static/others.js\" type=\"text/javascript\" defer></script>
<link rel=\"icon\" href=\"static/favicon.ico\">")

  (setq org-static-blog-page-preamble                       ; HTML to put before the content of each page.
        "<div id=\"blog-nav\">
<a href=\"https://lemyx.github.io/index.html\">Home</a>
<a href=\"https://lemyx.github.io/tags.html\">Tags</a>
<a href=\"https://lemyx.github.io/rss.xml\">Feeds</a>
<a href=\"https://lemyx.github.io/about.html\">About</a></div>")

  (defun org-static-blog-get-preview (post-filename)
    "Get title, date, tags from POST-FILENAME and get the first paragraph from the rendered HTML.
If the HTML body contains multiple paragraphs, include only the first paragraph,
and display an ellipsis.
Preamble and Postamble are excluded, too."
    (with-temp-buffer
      (insert-file-contents (org-static-blog-matching-publish-filename post-filename))
      (let ((post-title (org-static-blog-get-title post-filename))
            (post-date (org-static-blog-get-date post-filename))
            (preview-region (org-static-blog--preview-region)))
        ;; Put the substrings together.
        (let ((title-link
               (format "<h2 class=\"post-title\"><a href=\"%s\">%s</a></h2>"
                       (org-static-blog-get-post-url post-filename) post-title))
              (date-link
               (format-time-string (concat "<div class=\"post-date\">"
                                           (org-static-blog-gettext 'date-format)
                                           "</div>")
                                   post-date)))
          (concat
           title-link
           preview-region
           date-link
           "<hr class=\"post-divider\">"
           )))))

  (defun org-static-blog-assemble-multipost-page (pub-filename post-filenames &optional front-matter)
    "Assemble a page that contains multiple posts one after another.
Posts are sorted in descending time."
    (setq post-filenames (sort post-filenames (lambda (x y) (time-less-p (org-static-blog-get-date y)
                                                                         (org-static-blog-get-date x)))))
    (org-static-blog-with-find-file
     pub-filename
     (org-static-blog-template
      org-static-blog-publish-title
      (concat
       (when front-matter front-matter)
       (apply 'concat (mapcar
                       (if org-static-blog-use-preview
                           'org-static-blog-get-preview
                         'org-static-blog-get-post-content) post-filenames))))))

  (defun org-static-blog-assemble-multipost-page (pub-filename post-filenames &optional front-matter)
    "Assemble a page that contains multiple posts one after another.
Posts are sorted in descending time."
    (setq post-filenames (sort post-filenames (lambda (x y) (time-less-p (org-static-blog-get-date y)
                                                                         (org-static-blog-get-date x)))))
    (org-static-blog-with-find-file
     pub-filename
     (org-static-blog-template
      org-static-blog-publish-title
      (concat
       (when front-matter front-matter)
       (apply 'concat (mapcar
                       (if org-static-blog-use-preview
                           'org-static-blog-get-preview
                         'org-static-blog-get-post-content) post-filenames))))))

  (defun org-static-blog-assemble-tags ()
    "Render the tag archive and tag pages."
    (org-static-blog-assemble-tags-archive)
    (dolist (tag (org-static-blog-get-tag-tree))
      (org-static-blog-assemble-multipost-page
       (concat-to-dir org-static-blog-publish-directory
		              (concat "tag-" (downcase (car tag)) ".html"))
       (cdr tag)
       (concat "<h1 class=\"title\">" "「" (car tag) "」</h1>"))))

  (defun org-static-blog-assemble-tags-archive ()
    "Assemble the blog tag archive page.
The archive page contains single-line links and dates for every
blog post, sorted by tags, but no post body."
    (let ((tags-archive-filename (concat-to-dir org-static-blog-publish-directory org-static-blog-tags-file))
          (tag-tree (org-static-blog-get-tag-tree)))
      (setq tag-tree (sort tag-tree (lambda (x y) (string-greaterp (car y) (car x)))))
      (org-static-blog-with-find-file
       tags-archive-filename
       (org-static-blog-template
        org-static-blog-publish-title
        (concat
         (apply 'concat (mapcar 'org-static-blog-assemble-tags-archive-tag tag-tree)))))))

  (defun org-static-blog-get-post-summary (post-filename)
    "Assemble post summary for an archive page.
This function is called for every post on the archive and
tags-archive page. Modify this function if you want to change an
archive headline."
    (concat
     "<h2 class=\"post-title\">"
     "<a href=\"" (org-static-blog-get-post-url post-filename) "\">" (org-static-blog-get-title post-filename) "</a>"
     "</h2>\n"))

  (defun org-static-blog-assemble-tags-archive-tag (tag)
    "Assemble single TAG for all filenames."
    (let ((post-filenames (cdr tag)))
      (setq post-filenames
	        (sort post-filenames (lambda (x y) (time-less-p (org-static-blog-get-date x)
						                                    (org-static-blog-get-date y)))))
      (concat "<h1 class=\"tags-title\">" "「" (downcase (car tag)) "」</h1>\n"
	          (apply 'concat (mapcar 'org-static-blog-get-post-summary post-filenames)))))
)

(provide 'init-blog)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-blog.el ends here
