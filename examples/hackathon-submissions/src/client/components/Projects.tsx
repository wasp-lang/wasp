import React from 'react';
import { useQuery } from '@wasp/queries';
import getProjects from '@wasp/queries/getProjects';
import betathonLogo from '../betathonLogo.png';

const Projects = () => {
  const { data: projects, status } = useQuery(getProjects);

  return (
    <>
      <div className='bg-neutral-100'>
        <div className='mx-auto max-w-7xl px-4 sm:px-6 lg:px-8'>
          <div className='mx-auto max-w-2xl pb-16 lg:max-w-none'>
            <h2 className='text-2xl text-neutral-700 underline decoration-yellow-500 text-center'>
              Submitted Projects{' '}
            </h2>
            <div className='mt-12 gap-6 lg:grid lg:grid-cols-3'>
              {status === 'success' && projects.length ? (
                projects.map((project) => (
                  <div key={project.name} className='group relative'>
                    <div className='relative h-80 w-full overflow-hidden rounded-lg bg-white group-hover:opacity-75 sm:aspect-w-2 sm:aspect-h-1 sm:h-64 lg:aspect-w-1 lg:aspect-h-1'>
                      <img
                        src={project.image || betathonLogo}
                        alt={project.name}
                        className='h-full w-full object-cover object-center'
                      />
                    </div>
                    <h3 className='mt-6 text-base font-semibold text-neutral-700'>
                      <a href={!!project.website ? '//' + project.website : null} target='_blank' rel='noreferrer'>
                        <span className='absolute inset-0' />
                        {project.name}
                      </a>
                    </h3>
                    <p className='text-sm text-neutral-700'>
                      {project.description.length > 120
                        ? project.description.substring(0, 142).concat('...')
                        : project.description}
                    </p>
                  </div>
                ))
              ) : (
                <div className='col-span-3 border border-yellow-500/25 rounded-md bg-yellow-100 bg-opacity-10 shadow-md p-5 text-center  justify-self-center'>
                  <p className='font-semibold'>Nothing to see here... yet</p>
                  <p className='text-sm mt-4'>
                    Looking for some inspiration? <br />
                    Check out our{' '}
                    <a
                      className='text-yellow-600 underline decoration-neutral-700 '
                      href='https://github.com/wasp-lang/wasp/tree/main/examples'
                    >
                      {' '}
                      Example Wasp Apps
                    </a>{' '}
                    <p className='text-sm mt-4'>
                      BTW, This is also a Wasp App! 🧙‍♂️
                    </p>
                  </p>
                </div>
              )}
            </div>
          </div>
        </div>
      </div>
    </>
  );
};
export default Projects;
