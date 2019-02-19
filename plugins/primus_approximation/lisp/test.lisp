(require math)

(defmethod init ()
  (msg "hello from math $0" (sin 4611686018427387904:64))
  (msg "hello from math - sin(316.159265358979326) = $0" (sin 4644269548807471931:64))
  (msg "hello from math sin(7853981635.97448254) = $0" (sin 4755029503896426363:64)))


