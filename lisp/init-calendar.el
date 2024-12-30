;;; init-calendar.el --- Calendar settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package calendar
  :ensure nil
  :hook (calendar-today-visible . calendar-mark-today)
  :custom
  ;; 是否显示中国节日，我们使用 `cal-chinese-x' 插件
  (calendar-chinese-all-holidays-flag nil)
  ;; 是否显示节日
  (calendar-mark-holidays-flag t)
  ;; 是否显示Emacs的日记，我们使用org的日记
  (calendar-mark-diary-entries-flag nil)
  ;; 数字方式显示时区，如 +0800，默认是字符方式如 CST
  (calendar-time-zone-style 'numeric)
  ;; 日期显示方式：year/month/day
  (calendar-date-style 'iso)
  ;; 中文天干地支设置
  (calendar-chinese-celestial-stem ["甲" "乙" "丙" "丁" "戊" "己" "庚" "辛" "壬" "癸"])
  (calendar-chinese-terrestrial-branch ["子" "丑" "寅" "卯" "辰" "巳" "午" "未" "申" "酉" "戌" "亥"])
  ;; 设置中文月份
  (calendar-month-name-array ["一月" "二月" "三月" "四月" "五月" "六月" "七月" "八月" "九月" "十月" "十一月" "十二月"])
  ;; 设置星期标题显示
  (calendar-day-name-array ["日" "一" "二" "三" "四" "五" "六"])
  ;; 周一作为一周第一天
  (calendar-week-start-day 1)
  )

;; 中国节日设置
(use-package cal-china-x
  :ensure t
  :commands cal-china-x-setup
  :hook (after-init . cal-china-x-setup)
  :config
  ;; 重要节日设置
  (setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
  ;; 所有节日设置
  (setq cal-china-x-general-holidays
        '(;;公历节日
          (holiday-fixed 1 1 "元旦")
          (holiday-fixed 2 14 "情人节")
          (holiday-fixed 3 8 "妇女节")
          (holiday-fixed 5 1 "劳动节")
          (holiday-fixed 5 4 "青年节")
          (holiday-float 5 0 2 "母亲节")
          (holiday-fixed 6 1 "儿童节")
          (holiday-float 6 0 3 "父亲节")
          (holiday-fixed 9 10 "教师节")
          (holiday-fixed 10 1 "国庆节")
          ;; 农历节日
          (holiday-lunar 12 30 "除夕" 0)
          (holiday-lunar 1 1 "春节" 0)
          (holiday-lunar 1 15 "元宵节" 0)
          (holiday-solar-term "清明" "清明")
          (holiday-solar-term "小寒" "小寒")
          (holiday-solar-term "大寒" "大寒")
          (holiday-solar-term "立春" "立春")
          (holiday-solar-term "雨水" "雨水")
          (holiday-solar-term "惊蛰" "惊蛰")
          (holiday-solar-term "春分" "春分")
          (holiday-solar-term "谷雨" "谷雨")
          (holiday-solar-term "立夏" "立夏")
          (holiday-solar-term "小满" "小满")
          (holiday-solar-term "芒种" "芒种")
          (holiday-solar-term "夏至" "夏至")
          (holiday-solar-term "小暑" "小暑")
          (holiday-solar-term "大暑" "大暑")
          (holiday-solar-term "立秋" "立秋")
          (holiday-solar-term "处暑" "处暑")
          (holiday-solar-term "白露" "白露")
          (holiday-solar-term "秋分" "秋分")
          (holiday-solar-term "寒露" "寒露")
          (holiday-solar-term "霜降" "霜降")
          (holiday-solar-term "立冬" "立冬")
          (holiday-solar-term "小雪" "小雪")
          (holiday-solar-term "大雪" "大雪")
          (holiday-solar-term "冬至" "冬至")
          (holiday-lunar 5 5 "端午节" 0)
          (holiday-lunar 8 15 "中秋节" 0)
          (holiday-lunar 7 7 "七夕情人节" 0)
          (holiday-lunar 12 8 "腊八节" 0)
          (holiday-lunar 9 9 "重阳节" 0)))
  ;; 设置日历的节日，通用节日已经包含了所有节日
  (setq calendar-holidays (append cal-china-x-general-holidays)))

(provide 'init-calendar)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-calendar.el ends here
