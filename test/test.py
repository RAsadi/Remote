import subprocess
import unittest
import sys

from glob import glob
from pathlib import Path


class TestCompiler(unittest.TestCase):
    def __init__(self, test_file: str, out_file):
        self.test_file = Path(test_file)
        self.out_file = Path(out_file)
        setattr(self, self.test_file.stem, self.runTest)
        super().__init__(self.test_file.stem)

    def parse(self, out_file):
        line1 = out_file.readline()
        line1.strip()
        return int(line1)

    def runTest(self):
        with open(self.out_file) as out_file:
            # first, try to compile it using our compiler, and gcc
            compile_res = subprocess.run(
                ["dune", "exec", "remote", self.test_file],
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
            )
            pruned_filename = self.test_file.stem
            compile_output_file = f"build/{pruned_filename}"

            parsed_outfile = self.parse(out_file)

            # TODO add ret code to out file
            if compile_res.returncode == 0:
                our_compiled_res = subprocess.run(
                    [compile_output_file], stdout=subprocess.PIPE, timeout=3
                )
                self.assertEqual(parsed_outfile, our_compiled_res.returncode)
            else:
                raise Exception(f"{pruned_filename}: {compile_res.stderr}")


def suite():
    test_suite = unittest.TestSuite()
    folders = glob("test/data/*")

    for folder in folders:
        # TODO change once multifile support
        rc_file = glob(f"{folder}/*.rc")[0]
        out_file = f"{folder}/.out"
        test_suite.addTest(TestCompiler(rc_file, out_file))
    return test_suite


if __name__ == "__main__":
    unittest.TextTestRunner().run(suite())
