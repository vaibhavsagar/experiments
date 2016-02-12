import zlib
import hashlib
import os
from binascii import hexlify, unhexlify


def sha1_to_path(sha1, directory):
    prefix = sha1[:2]
    suffix = sha1[2:]
    path = '/'.join([directory, 'objects', prefix, suffix])
    return path


def sha1_to_directory(sha1, directory):
    prefix = sha1[:2]
    path = '/'.join([directory, 'objects', prefix])
    return path


def disambiguate_sha1(sha1, directory):
    prefix = sha1[:2]
    suffix = sha1[2:]
    matching_files = matching_files = [
        file for file in
        os.listdir(sha1_to_directory(sha1, directory))
        if file.startswith(suffix)]
    if len(matching_files) == 0:
        raise Exception('No object exists with that SHA1.')
    elif len(matching_files) > 1:
        raise Exception('Ambiguous SHA1 provided.')
    else:
        return prefix + matching_files[0]


def pretty_print(sha1, directory='.git'):
    if len(sha1) < 40:
        sha1 = disambiguate_sha1(sha1, directory)
    path = sha1_to_path(sha1, directory)
    with open(path, mode='rb') as compressed:
        decompressed = zlib.decompress(compressed.read())
    entries = decompressed.split(b'\x00')
    header, *content = entries
    object_type = header.split(b' ')[0]
    if object_type == b'blob':
        return [entry.decode() for entry in [header, *content]]
    elif object_type == b'tree':
        objects = []
        for i, obj in enumerate(content):
            if i == len(content)-1:
                objects.append(hexlify(obj))
            elif i == 0:
                objects.append(obj)
            else:
                hash, name = obj[:20], obj[20:]
                objects.append(hexlify(hash))
                objects.append(name)
        return [entry.decode() for entry in [header, *objects]]


class GitObject:

    def __init__(self, content):
        self.content = content

    @property
    def sha1(self):
        hash = hashlib.sha1()
        hash.update(self.store)
        return hash.hexdigest()


class NamedObject:

    def __init__(self, content, name):
        self.content = content
        self.name = name

    @property
    def entry(self):
        mode_bytestring = '{:o}'.format(self.mode).zfill(6).encode()
        return b''.join([mode_bytestring, b' ', self.name.encode(),
                         b'\x00', unhexlify(self.sha1)])


class Blob(GitObject):

    @property
    def store(self):
        header = 'blob ' + str(len(self.content)) + '\x00'
        store = header + self.content
        return store.encode()

    def __repr__(self):
        return 'Blob(' + repr(self.content) + ')'


class NamedBlob(NamedObject, Blob):
    mode = 0o100644


class Tree(GitObject):
    @property
    def store(self):
        entries = []
        for obj in sorted(self.content, key=lambda entry: entry.name):
            entries.append(obj.entry)
        content = b''.join(entries)
        header = 'tree ' + str(len(content)) + '\x00'
        store = header.encode() + content
        return store


class NamedTree(NamedObject, Tree):
    mode = 0o040000


def store(obj, directory='.git'):
    sha1 = obj.sha1
    path = sha1_to_path(sha1, directory)
    if os.path.isfile(path):
        # Given how unlikely it is that there has been a hash collision,
        # assume the user is writing the same blob again.
        pass
    else:
        os.makedirs(sha1_to_directory(sha1, directory), exist_ok=True)
        with open(path, mode='wb') as file:
            file.write(zlib.compress(obj.store))
    return sha1
