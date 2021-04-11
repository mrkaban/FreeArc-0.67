#include "../Compression.h"

// Максимальный размер ключа/iv/хеша в байтах
#define MAXKEYSIZE 64

// Быстрая ad-hoc проверка того, что алгоритм включает методы шифрования
bool compressorIsEncrypted_Guess (char *compressor)
{
  char c[MAX_COMPRESSOR_STRLEN] = "+";
  strcpy(c+1, compressor);
  return (strstr (c, "+aes-")!=NULL || strstr (c, "+serpent-")!=NULL || strstr (c, "+blowfish-")!=NULL || strstr (c, "+twofish-")!=NULL);
}


enum TEncrypt {ENCRYPT, DECRYPT};
int docrypt (enum TEncrypt DoEncryption, int cipher, int mode, BYTE *key, int keysize, int rounds, BYTE *iv,
             CALLBACK_FUNC *callback, void *auxdata);


#ifdef __cplusplus

// Реализация стандартного интерфейса методов сжатия/шифрования COMPRESSION_METHOD
class ENCRYPTION_METHOD : public COMPRESSION_METHOD
{
public:
  // Параметры этого метода шифрования
  int  cipher;                   // ИД алгоритма шифрования
  int  mode;                     // ИД режима шифрования (ctr, cfb...)
  char key [MAXKEYSIZE*2+1];     // Ключ в base16 записи
  char iv  [MAXKEYSIZE*2+1];     // Вектор инициализации в base16 записи
  char salt[MAXKEYSIZE*2+1];     // "Соль" в base16 записи
  char code[MAXKEYSIZE*2+1];     // Код самопроверки в base16 записи
  int  numIterations;            // Количество итераций при генерации ключа по password+salt
  int  rounds;                   // Количество шагов при шифровании, чем больше - тем медленней, но надёжней. 0 - использовать знаечние по умолчанию, рекомендованное создателями алгоритма
  int  keySize;                  // Длина ключа в байтах (0 - использовать максимальную доступную)
  int  ivSize;                   // Длина IV в байтах (=размеру блока алгоритма шифрования)
  bool fixed;                    // Шифрование выполнено с корректным decode16

  // Конструктор, присваивающий параметрам метода шифрования значения по умолчанию
  ENCRYPTION_METHOD();

  // Универсальный метод, отвечает на запросы "encryption?", "KeySize" и "IVSize"
  virtual int doit (char *what, int param, void *data, CALLBACK_FUNC *callback);
  // Функции распаковки и упаковки
  virtual int decompress (CALLBACK_FUNC *callback, void *auxdata);
#ifndef FREEARC_DECOMPRESS_ONLY
  virtual int compress   (CALLBACK_FUNC *callback, void *auxdata);

  // Получить/установить объём памяти, используемой при упаковке/распаковке, размер словаря или размер блока
  virtual MemSize GetCompressionMem        (void)               {return LARGE_BUFFER_SIZE;}
  virtual void    SetCompressionMem        (MemSize mem)        {}
  virtual void    SetMinDecompressionMem   (MemSize)            {}
#endif
  virtual MemSize GetDecompressionMem      (void)               {return LARGE_BUFFER_SIZE;}

  // Записать в buf[MAX_METHOD_STRLEN] строку, описывающую метод сжатия и его параметры (функция, обратная к parse_ENCRYPTION)
  virtual void ShowCompressionMethod (char *buf, bool purify);
};

// Разборщик строки метода шифрования
COMPRESSION_METHOD* parse_ENCRYPTION (char** parameters);

#endif  // __cplusplus
