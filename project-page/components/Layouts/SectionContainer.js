import classNames from 'classnames'

const SectionContainer = ({ children, className }) => (
  <div 
    className={classNames(
      'container mx-auto px-6 py-16 sm:py-18',
      'md:py-24',
      'lg:px-16 lg:py-24 xl:px-20',
      className
    )}
  >
    { children }
  </div>
)

export default SectionContainer
