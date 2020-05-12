# command-of-the-day

This is an Emacs package to help you sharp your Emacs skills. You
should plan your week ahead or just practice whatever key bindings do
you like.

# Installation

This package is not in MELPA yet. You need to grab this and place in
your 'load-path.

```elisp
(require 'command-of-the-day)
(command-of-the-day +1)
```

You can get your current report using `M-x
command-of-the-day-report`. For now, the report is just a json pretty
printed representation.

# Customizations

You should setup the bindings you desire to follow:

```elisp
(setq command-of-the-day-practice-schedule (list :monday (list "C-v" "M-v")
      					   	 :tuesday (list "C-x 8 {")
						 :any (list "C-)")))
```

The `:any` keyword will be used as fallback to any other day that is
not specified.


You can also change how many hits do you need to gradually move into the mastery path:

```elisp
(setq command-of-the-day-apprentice-level 3
      command-of-the-day-journeyman-level 10)

;;; how long the message will appear in the mode-line when you hit the key.
(setq command-of-the-day-user-feedback-time 3)
```

# Felicitous Use

One day the master macrologist Pi Mu and his macrolyte were having a
meal of saba roasted on a shichirin when Pi Mu suddenly reached in
with his chopsticks and extracted a red hot coal. He presented to the
macrolyte stating “for you.” The macrolyte not knowing what to do but
also not wishing to burn himself pulled his sleeves down over his
hands and grabbed for the coal. Of course, the intense heat still hurt
and the student ran to the kitchen and drenched the coal under running
water. The macrolyte was dismayed to find his sleeve stained and
burned from the coal and was quite angry as a result. In his anger he
went back to the dining room and reached into the fire with his own
chopsticks and extracted a coal of his own and presented the coal to
Pi Mu. Pi Mu immediately pull from his jacket a cigar and lit it on
the coal, saying “thank you, I needed that.”

The student was enlightened.

(stolen from Read-Eval-print-λove from Michael Fogus, please read it.)

# License
Copyright (C) 2020-2020 Wanderson Ferreira

Author: Wanderson Ferreira wanderson.ferreira@protonmail.com Keywords: keys bindings

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program. If not, see http://www.gnu.org/licenses/.
