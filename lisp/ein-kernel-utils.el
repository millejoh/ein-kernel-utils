;;; ein-kernel-utils.el --- Kernel-based utilities for the Emacs IPython Notebook -*- lexical-binding: t; -*-

;; Copyright (C) 2020 John Miller

;; Author: John Miller <millejoh at mac dot com>
;; Maintainer: John Miller <millejoh at mac dot com>
;; Version: 0.1
;; Keywords: ein, python
;; URL: https://github.com/millejoh/ein-kernel-utils
;; Package-Requires: ((ein))

;; This file is NOT part of GNU Emacs.

;; ein-kernel-utils.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-multilang.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-kernel-utils.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ein-classes)
(require 'ein-company)

(defconst ein-utils:source-dir (file-name-directory load-file-name)
  "Directory in which `ein*.el` files are located.")

(defun ein:kernel-language (kernel)
  (ein:$kernelspec-language (ein:$kernel-kernelspec kernel)))

(provide 'ein-kernel-utils)

;;; ein-kernel-utils.el ends here
