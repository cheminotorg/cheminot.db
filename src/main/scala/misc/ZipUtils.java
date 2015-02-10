package m.cheminot.misc;

import java.io.*;
import java.net.URI;
import java.util.*;
import java.util.zip.*;

public class ZipUtils {

  public static void zip(File directory, File zipfile) throws IOException {
    URI base = directory.toURI();
    Deque<File> queue = new LinkedList<File>();
    queue.push(directory);
    OutputStream out = new FileOutputStream(zipfile);
    Closeable res = out;
    try {
      ZipOutputStream zout = new ZipOutputStream(out);
      res = zout;
      while (!queue.isEmpty()) {
        directory = queue.pop();
        for (File kid : directory.listFiles()) {
          java.lang.String name = base.relativize(kid.toURI()).getPath();
          if (kid.isDirectory()) {
            queue.push(kid);
            name = name.endsWith("/") ? name : name + "/";
            zout.putNextEntry(new ZipEntry(name));
          } else {
            zout.putNextEntry(new ZipEntry(name));
            copy(kid, zout);
            zout.closeEntry();
          }
        }
      }
    } finally {
      res.close();
    }
  }

  public static void zip(Iterable<File> files, File zipfile) throws IOException {
    OutputStream out = new FileOutputStream(zipfile);
    Closeable res = out;
    try {
      ZipOutputStream zout = new ZipOutputStream(out);
      res = zout;
      for (File kid : files) {
          java.lang.String name = kid.getName();
          if (kid.isDirectory()) {
              throw new RuntimeException("Directories not supported in zipping a list of files");
          } else {
              zout.putNextEntry(new ZipEntry(name));
              copy(kid, zout);
              zout.closeEntry();
          }
      }
    }
    finally {
      res.close();
    }
  }


  public static void unzip(File zipfile, File directory) throws IOException {
    ZipFile zfile = new ZipFile(zipfile);
    Enumeration<? extends ZipEntry> entries = zfile.entries();
    while (entries.hasMoreElements()) {
      ZipEntry entry = entries.nextElement();
      File file = new File(directory, entry.getName());
      if (entry.isDirectory()) {
        file.mkdirs();
      } else {
        file.getParentFile().mkdirs();
        InputStream in = zfile.getInputStream(entry);
        try {
          copy(in, file);
        } finally {
          in.close();
        }
      }
    }
  }


  private static void copy(InputStream in, OutputStream out) throws IOException {
    byte[] buffer = new byte[1024];
    while (true) {
      int readCount = in.read(buffer);
      if (readCount < 0) {
        break;
      }
      out.write(buffer, 0, readCount);
    }
  }

  private static void copy(File file, OutputStream out) throws IOException {
    InputStream in = new FileInputStream(file);
    try {
      copy(in, out);
    } finally {
      in.close();
    }
  }

  private static void copy(InputStream in, File file) throws IOException {
    OutputStream out = new FileOutputStream(file);
    try {
      copy(in, out);
    } finally {
      out.close();
    }
  }
}
