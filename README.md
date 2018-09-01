<p>Mixalot is a collection of systems related to audio in Common Lisp,
under an MIT-style license. Currently it consists of a mixer component
providing real-time audio output on Linux (via ALSA) and other
platforms (using <a href="http://xiph.org/ao/">libao</a>),
<a href="http://common-lisp.net/project/cffi/">CFFI</a> bindings to
the <a href="http://www.mpg123.de/api/">libmpg123</a>, <a href="http://flac.sourceforge.net/">libFLAC</a>,
and <a href="http://xiph.org/vorbis/doc/vorbisfile/index.html">libvorbisfile</a>
libraries, and audio stream classes for simple playback of MP3, Ogg,
and FLAC files through the mixer.</p>

The documentation is available at [http://vintage-digital.com/hefner/software/mixalot/mixalot.html](http://vintage-digital.com/hefner/software/mixalot/mixalot.html)

This library is for example used in the
[Shuffletron](https://github.com/ahefner/shuffletron/) music player.

<!--  ----------------------------------------------------------------- -->
<h2><a name="Installation"></a>Installation</h2>

<p>The most recent development version of Mixalot is hosted on <a href="http://github.com/ahefner/mixalot">github</a> and can be obtained as follows:</p>
<pre class="lisp">
git clone git://github.com/ahefner/mixalot.git</pre>

<p>Mixalot includes several ASDF systems which should be symlinked into the ASDF central registry in the usual fashion. It depends directly on the following systems:</p>
<ul>
  <li>CFFI</li>
  <li>bordeaux-threads</li>
  <li>Alexandria</li>
</ul>

<p>You may find this library useful for the following purposes:</p>
<ul>
  <li>Audio output, if you aren't overly concerned with latency</li>
  <li>Playing MP3, Ogg, or FLAC files</li>
  <li>Decoding audio files to a vector in memory</li>
  <li>Querying ID3 tags from MP3 files</li>
  <li>Querying tags from FLAC and Ogg Vorbis files</li>
</ul>


<!--  ----------------------------------------------------------------- -->
<h2><a name="Portability"></a>Portability</h2>

<p>The CFFI and bordeaux-threads libraries are used to ease porting
between lisp implementations. The mixer component of Mixalot chooses
at compile time to use ALSA (on Linux)
or <a href="http://xiph.org/ao/">libao</a> (everywhere else) and
should be able to produce audio on any platform supported by libao. In
order to be useful in an application, the mixer must also run in its
own (native) thread.  Therefore, the <tt>mixalot</tt>
and <tt>mixalot-mp3</tt> systems should be usable on CL
implementations capable of running multiple threads, on Linux or
platforms supported by libao. It has been tested and is known to work
in the following configurations:</p>
<ul>
  <li>SBCL (w/sb-thread enabled) on Linux/x86 and Linux/x86-64</li>
  <li>SBCL (w/sb-thread enabled) on Mac OS X 10.6.3 (x86_64)</li>
  <li>SBCL (w/sb-thread enabled) on FreeBSD 8.2 (amd64)</li>
  <li>Clozure CL on Linux/x86_64</li>
</ul>
<p>The <tt>mpg123-ffi</tt> system is independent and should be usable on any CL supported by CFFI.</p>
