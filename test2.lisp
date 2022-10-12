(ql:quickload :cl-portaudio)
(ql:quickload :vgplot)

(defpackage :clynth
  (:use :cl)
  (:export :linear-slide))

(in-package clynth)

(defconstant +sample-rate+ 44100)
(defconstant +buffer-size+ 1024)
(defconstant +pi+ (coerce pi 'single-float))

(defvar astream nil)

(defun curry (fn &rest args)
  (lambda (&rest remaining-args)
    (apply fn (append args remaining-args))))

(defun start ()
  (unless astream
    (portaudio::initialize)
    (setq astream (portaudio::open-default-stream 0 1 :float (coerce  +sample-rate+ 'double-float) +buffer-size+))
    (portaudio::start-stream astream)))

(defun stop ()
  (when astream
    (portaudio::stop-stream astream)
    (setq astream nil)
    (portaudio::terminate)))

(defun play-buffer (buffer)
  "Play longer buffer"
  (let ((nbuffers (floor (/ (length buffer)
                            +buffer-size+))))
    (dotimes (i nbuffers)
      (portaudio::write-stream astream
                    (subseq buffer
                            (* i +buffer-size+)
                            (* (+ i 1) +buffer-size+))))))

(defun make-buffer (nsamples)
  (make-array (+ nsamples (mod nsamples +buffer-size+)) :element-type 'single-float))

(defun collect (producer nsamples)
  (let ((buffer (make-buffer nsamples)))
    (loop for i upto (- nsamples 1) do
      (setf (aref buffer i) (funcall producer)))
    buffer))

(defun frequency-to-delta (frequency)
  (/ frequency +sample-rate+))

(defun make-sampler (func)
  (let ((phase 0.0))
    (lambda (delta) (funcall func (incf phase delta)))))

(defun make-periodic-sampler (frequency func &key (period 1.0))
  (let ((phase 0.0))
    (lambda ()
      (setq phase (mod (+ phase (* period (frequency-to-delta (funcall frequency)))) period))
      (funcall func phase))))

(defun plot-buffer (buffer)
  (vgplot:plot buffer ";use of additional styles;with points pt 7 ps 1 lc 'red'"))

(defun triangle-wave (x) (1- (* 4 (abs (- (+ x 0.25)
                                         (floor (+ x 0.75)))))))

(defun make-sine-wave (frequency) (make-periodic-sampler frequency #'sin :period (* 2 +pi+)))
(defun make-sawtooth-wave (frequency) (make-periodic-sampler frequency (lambda (x) (1- (* 2 (mod (+ x .5) 1))))))
(defun make-triangle-wave (frequency) (make-periodic-sampler frequency #'triangle-wave))
(defun make-block-wave (frequency) (make-periodic-sampler frequency (lambda (x) (1- (* 2.0 (round (mod (- x .75) 1.0)))))))

(defun constant (n) (lambda () n))

(defun seconds (secs)
  (round (* secs +sample-rate+)))

;; (plot-buffer (collect (make-sine-wave (constant 440)) (seconds 0.1)))

(defmacro lam-let (producers transformer)
  `(let* ,producers
     (lambda ()
       (let ,(mapcar (lambda (pair) `(,(car pair) (funcall ,(car pair)))) producers)
         ,transformer))))

(defvar swooper1
      (lam-let ((frequency-modulator (lam-let ((x (make-sine-wave (constant 10))))
                                              (+ 440 (* 120 x))))
                (amplitude-modulator (make-sine-wave (constant 5)))
                (oscillator (make-sine-wave frequency-modulator)))
           (* oscillator amplitude-modulator)))



(defun play (producer nsamples)
  (play-buffer (collect producer nsamples)))

(defun playx ()
  (start)
  (play swooper1 (seconds 1))
  (play (curry (make-sawtooth-wave) (frequency-to-delta 440)) (seconds 1))
  ;; (play (curry (make-sine-wave) (frequency-to-delta 440)) (seconds 1))
  ;; (play (curry (make-triangle-wave) (frequency-to-delta 440)) (seconds 1))
  ;; (play (curry (make-block-wave) (frequency-to-delta 440)) (seconds 1))
  ;; (play (swooper 140 15 155.0) (seconds 1.5))
  ;; (play (swooper 140 5 25.0) (seconds 1.5))
  (stop))

;; (defun test-plot ()
;;   (plot-buffer (collect (curry (make-block-wave) (frequency-to-delta 10.0)) (seconds 1)))
;;   )

(defun iter (input)
  (let ((remaining input))
    (lambda ()
      (let ((current (car remaining)))
        (setq remaining (cdr remaining))
        current))))

(defun linear-slide (start stop duration)
  (let ((current 0))
    (lambda ()
      (unless (>= current duration)
        (+ start (* (/ (incf current) duration) (- stop start)))))))

;; (defthing linear-slide (start stop :input duration))
;; (defun $linear-slider (start stop duration)
;;   ($ start + (x / duration) * (stop - start)))

;; (defvar linear-slider (sampler $linear-slider))

(defun concat-sequence (&rest producers)
  (let ((remaining-producers producers))
    (labels ((get-next ()
               (let ((producer (car remaining-producers)))
                 (when producer
                   (or (funcall producer)
                       (progn (setq remaining-producers (cdr remaining-producers)) (get-next)))))))
      #'get-next)))

(defun adsr-envelope (input attack-time decay-time sustain-level release-time)
  (let ((current-producer nil)
        (previous-input nil))
    (lambda ()
      (let ((input (funcall input)))
        (cond ((and input (not previous-input))
               ;; note played
               (setq current-producer (concat-sequence (linear-slide 0 1 attack-time)
                                                       (linear-slide 1 sustain-level decay-time))))
              ((and previous-input (not input))
               ;; note released
               (setq current-producer (concat-sequence current-producer
                                                       (linear-slide sustain-level 0 release-time)))))


        (setq previous-input input)

        (when current-producer
          (if input (or (funcall current-producer)
                        ;; either in attack/decay, otherwise sustain
                        sustain-level)
              (funcall current-producer)))))))

(defun piano (input)
  (lam-let ((envelope (adsr-envelope input (seconds 0.01) (seconds 0.2) 0.3 (seconds 0.2)))
            (oscillator (make-sine-wave (constant 440))))
           (* oscillator (or envelope 0))))

(defun piano-notes ()
  (let ((note-sequence (append (make-list 3 :initial-element 1))))
    (piano (iter note-sequence))))

(defun testje () ()
  ;; (setq note-sequence (append (make-list 5100 :initial-element 1) '(nil 1)))
  (play (piano-notes) (seconds 1))
  (plot-buffer (collect (piano-notes) (seconds 1))))

;; ((collect (notes) (seconds 1)))
;; (sb-sprof:with-profiling (:report :graph :max-samples 1000)
;;   (collect (notes) (seconds 1)))

