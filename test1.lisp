(ql:quickload :cl-portaudio)
(ql:quickload :vgplot)

(defpackage :user
  (:use :cl))

(in-package user)

(defconstant +sample-rate+ 44100)
(defconstant +buffer-size+ 1024)
(defconstant +frames-per-buffer+ +buffer-size+)

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
                            +frames-per-buffer+))))
    (dotimes (i nbuffers)
      (portaudio::write-stream astream
                    (subseq buffer
                            (* i +frames-per-buffer+)
                            (* (+ i 1) +frames-per-buffer+))))))

(defun make-buffer (nsamples)
  (make-array (+ nsamples (mod nsamples +frames-per-buffer+)) :element-type 'single-float))

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

(defun make-periodic-sampler (func &key (period 1.0))
  (let ((phase 0.0))
    (lambda (delta) (funcall func (setq phase (mod (+ phase (* period delta)) period))))))


(defun plot-buffer (buffer)
  (vgplot:plot buffer))

(defun triangle-wave (x) (1- (* 4 (abs (- (+ x 0.25)
                                         (floor (+ x 0.75)))))))

(defun make-sine-wave () (make-periodic-sampler #'sin :period (* 2 +pi+)))
(defun make-sawtooth-wave () (make-periodic-sampler (lambda (x) (1- (* 2 (mod (+ x .5) 1))))))
(defun make-triangle-wave () (make-periodic-sampler #'triangle-wave))
(defun make-block-wave () (make-periodic-sampler (lambda (x) (1- (* 2.0 (round (mod (- x .75) 1.0)))))))

(defun multiply (producer modulator)
  (lambda ()
      (* (funcall producer) (funcall modulator))))

(defun make-amplitude-mod (producer)
 (lambda (amplitude) (* (producer) amplitude)))

(defun make-swooper (frequency-producer oscillator)
  (lambda () (funcall oscillator (frequency-to-delta (funcall frequency-producer)))))

(defvar swooper1 (let ((oscillator )) make-swooper (multiply ((curry make-sine-wave (frequency-to-delta))))))

(defun swooper (base-frequency modulator-frequency modulator-amplitude)
  (let ((oscillator (make-sawtooth-wave))
        (modulator (curry (make-sine-wave) (frequency-to-delta modulator-frequency))))
    (lambda ()
      (funcall oscillator
               (frequency-to-delta (+ base-frequency
                                      (* modulator-amplitude (funcall modulator))))))))




;; (sine wave => sine wave) => | mod amplitude
;; (sine wave)              => |
(defun seconds (secs)
  (round (* secs +sample-rate+)))

(defun play (producer nsamples)
  (play-buffer (collect producer nsamples)))

(defun playx ()
  (start)
  (play (curry (make-sawtooth-wave) (frequency-to-delta 440)) (seconds 1))
  (play (curry (make-sine-wave) (frequency-to-delta 440)) (seconds 1))
  (play (curry (make-triangle-wave) (frequency-to-delta 440)) (seconds 1))
  (play (curry (make-block-wave) (frequency-to-delta 440)) (seconds 1))
  (play (swooper 140 15 155.0) (seconds 1.5))
  (play (swooper 140 5 25.0) (seconds 1.5))
  (play (swooper2 (swooper 140 5 55.0) (curry (make-sine-wave) (frequency-to-delta 30))) (seconds 5))
  (stop))

(playx)

(defun test-plot ()
  (plot-buffer (collect (curry (make-block-wave) (frequency-to-delta 10.0)) (seconds 1)))
  )
