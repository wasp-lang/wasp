import React, { useState } from 'react';
import submitProject from '@wasp/actions/submitProject';

export type Submission = {
  name: string;
  email: string;
  country: string;
  website: string;
  github: string;
  twitter: string;
  description: string;
  image: string;
};

const SubmissionForm = () => {
  const [file, setFile] = useState<any>();
  const [isUploading, setIsUploading] = useState(false);
  const [imageLink, setImageLink] = useState('');

  const onFileUpload = async (event) => {
    setIsUploading(true);
    const clientId = 'd4ecb4220cf055b'
    const auth = 'Client-ID ' + clientId;

    const formData = new FormData();
    formData.append('image', event.target?.files[0]);

    try {
      const imgur = await fetch('https://api.imgur.com/3/upload', {
        method: 'POST',
        body: formData,
        headers: {
          Authorization: auth,
          Accept: 'application/json',
        },
      });

      const json = await imgur.json();

      if (!json.success) {
        throw new Error('Image upload failed');
      }
      setFile(event.target.files[0].name);
      setImageLink(json.data.link);
    } catch (error) {
      console.error('error uploading image');
    }
    setIsUploading(false);
  };

  const handleSubmit = async (event) => {
    event.preventDefault();
    const data = new FormData(event.target);
    const value = Object.fromEntries(data.entries());
    delete value['file-upload'];
    value.image = imageLink;

    try {
      await submitProject(value as Submission);
      alert('Project submitted successfully! It will be visible once it is approved.');
      event.target.reset();
    } catch (e) {
      console.error('Error while submitting project', e);
      alert('Error while submitting project');
    }
  };

  return (
    <>
      <div className='skew-y-min2 border-double border-2 border-t border-b border-yellow-500/25 w-full mb-10'></div>

      <div className='relative mt-10 md:pt-6 px-3 sm:mt-0 sm:px-20 sm:mx-10 lg:w-1/2' id='submission'>
        <h2 className='text-2xl text-neutral-700 mb-5 mx-1 underline decoration-yellow-500'>Submit a Project</h2>

        <div className='md:grid md:grid-cols-2 md:gap-6'>
          <div className='mt-5 md:col-span-2 md:mt-0'>
            <form onSubmit={handleSubmit} method='POST'>
              <div className='overflow-hidden shadow sm:rounded-md'>
                <div className='bg-white px-4 py-5 sm:p-6'>
                  <div className='grid grid-cols-6 gap-6'>
                    <div className='col-span-6'>
                      <label htmlFor='name' className='block text-sm font-medium text-gray-700'>
                        Project Name *
                      </label>
                      <input
                        type='text'
                        name='name'
                        id='name'
                        required
                        className='mt-1 block w-full rounded-md border-gray-300 shadow-sm focus:border-yellow-500 focus:ring-yellow-500 sm:text-sm'
                      />
                    </div>

                    <div className='col-span-6'>
                      <label htmlFor='email' className='block text-sm font-medium text-gray-700'>
                        Email address *
                      </label>
                      <input
                        type='text'
                        name='email'
                        id='email'
                        required
                        autoComplete='email'
                        className='mt-1 block w-full rounded-md border-gray-300 shadow-sm focus:border-yellow-500 focus:ring-yellow-500 sm:text-sm'
                      />
                    </div>

                    <div className='col-span-6'>
                      <label htmlFor='country' className='block text-sm font-medium text-gray-700'>
                        Countries Represented
                      </label>
                      <input
                        type='text'
                        name='country'
                        id='country'
                        autoComplete='country-name'
                        className='mt-1 block w-full rounded-md border-gray-300 shadow-sm focus:border-yellow-500 focus:ring-yellow-500 sm:text-sm'
                      />
                    </div>

                    <div className='col-span-6'>
                      <label htmlFor='github' className='block text-sm font-medium text-gray-700'>
                        GitHub Repo *
                      </label>
                      <div className='mt-1 flex rounded-md shadow-sm'>
                        <span className='inline-flex items-center rounded-l-md border border-r-0 border-gray-300 bg-gray-50 px-3 text-sm text-gray-500'>
                          http://
                        </span>
                        <input
                          type='text'
                          name='github'
                          id='github'
                          required
                          className='block w-full flex-1 rounded-none rounded-r-md border border-gray-300 focus:border-yellow-500 focus:ring-yellow-500 sm:text-sm'
                        />
                      </div>
                    </div>

                    <div className='col-span-6'>
                      <label htmlFor='website' className='block text-sm font-medium text-gray-700'>
                        Website
                      </label>
                      <div className='mt-1 flex rounded-md shadow-sm'>
                        <span className='inline-flex items-center rounded-l-md border border-r-0 border-gray-300 bg-gray-50 px-3 text-sm text-gray-500'>
                          http://
                        </span>
                        <input
                          type='text'
                          name='website'
                          id='website'
                          className='block w-full flex-1 rounded-none rounded-r-md border border-gray-300 focus:border-yellow-500 focus:ring-yellow-500 sm:text-sm'
                          placeholder='www.example.com'
                        />
                      </div>
                    </div>

                    <div className='col-span-6'>
                      <label htmlFor='twitter' className='block text-sm font-medium text-gray-700'>
                        Twitter
                      </label>
                      <div className='mt-1 flex rounded-md shadow-sm'>
                        <span className='inline-flex items-center rounded-l-md border border-r-0 border-gray-300 bg-gray-50 px-3 text-sm text-gray-500'>
                          http://
                        </span>
                        <input
                          type='text'
                          name='twitter'
                          id='twitter'
                          className='block w-full flex-1 rounded-none rounded-r-md border border-gray-300 focus:border-yellow-500 focus:ring-yellow-500 sm:text-sm'
                        />
                      </div>
                    </div>

                    <div className='col-span-6'>
                      <label htmlFor='description' className='block text-sm font-medium text-gray-700'>
                        Description *
                      </label>
                      <div className='mt-1'>
                        <textarea
                          id='description'
                          name='description'
                          rows={3}
                          className='mt-1 block w-full rounded-md border border-gray-300 shadow-sm focus:border-yellow-500 focus:ring-yellow-500 sm:text-sm'
                          placeholder='Our project is so WASPY because...'
                          required
                        ></textarea>
                      </div>
                    </div>

                    <div className='col-span-6'>
                      <label className='block text-sm font-medium text-gray-700'>Cover photo</label>
                      <div
                        className={`${
                          isUploading && 'pointer-events-none opacity-35'
                        } mt-1 flex justify-center rounded-md border-2 border-dashed border-gray-300 px-6 pt-5 pb-6`}
                      >
                        <div className={`${isUploading && 'animate-pulse'} space-y-1 text-center`}>
                          <svg
                            className='mx-auto h-12 w-12 text-gray-400'
                            stroke='currentColor'
                            fill='none'
                            viewBox='0 0 48 48'
                            aria-hidden='true'
                          >
                            <path
                              d='M28 8H12a4 4 0 00-4 4v20m32-12v8m0 0v8a4 4 0 01-4 4H12a4 4 0 01-4-4v-4m32-4l-3.172-3.172a4 4 0 00-5.656 0L28 28M8 32l9.172-9.172a4 4 0 015.656 0L28 28m0 0l4 4m4-24h8m-4-4v8m-12 4h.02'
                              strokeWidth='2'
                              strokeLinecap='round'
                              strokeLinejoin='round'
                            />
                          </svg>
                          <div className='flex text-sm text-gray-600 justify-center'>
                            <label
                              htmlFor='file-upload'
                              className='relative cursor-pointer rounded-md bg-white font-medium underline text-yellow-600 focus-within:outline-none hover:text-yellow-500 outline-none focus:outline-none'
                            >
                              {/** UPLOAD IMAGE */}
                              {isUploading ? (
                                <svg
                                  className='animate-spin -ml-1 mr-3 h-5 w-5 text-white'
                                  xmlns='http://www.w3.org/2000/svg'
                                  fill='none'
                                  viewBox='0 0 24 24'
                                >
                                  <circle
                                    className='opacity-25'
                                    cx='12'
                                    cy='12'
                                    r='10'
                                    stroke='black'
                                    strokeWidth='4'
                                  ></circle>
                                  <path
                                    className='opacity-75'
                                    fill='gray'
                                    d='M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z'
                                  ></path>
                                </svg>
                              ) : (
                                <span>Upload an Image</span>
                              )}
                              <input
                                id='file-upload'
                                name='file-upload'
                                type='file'
                                onChange={(e) => onFileUpload(e)}
                                className='sr-only'
                                accept='image/png, image/jpeg, image/jpg'
                              />
                            </label>
                            {!!file && (
                              <p className='pl-1'>{file} uploaded</p>
                            )}
                          </div>
                          <p className='text-xs text-gray-500'>PNG, JPG, GIF up to 10MB</p>
                        </div>
                      </div>
                    </div>
                  </div>
                </div>
                <div className='bg-gray-50 px-4 py-3 text-right sm:px-6'>
                  <button
                    type='submit'
                    disabled={isUploading}
                    className='inline-flex justify-center rounded-md border border-transparent bg-yellow-600 py-2 px-4 text-sm font-medium text-white shadow-sm disabled:opacity-60 enabled:hover:bg-yellow-700 focus:outline-none focus:ring-2 focus:ring-yellow-500 focus:ring-offset-2'
                  >
                    Save
                  </button>
                </div>
              </div>
            </form>
          </div>
        </div>
      </div>

      <div className='hidden sm:block' aria-hidden='true'>
        <div className='py-5'>
          <div className='border-t border-gray-200'></div>
        </div>
      </div>
    </>
  );
};

export default SubmissionForm;
